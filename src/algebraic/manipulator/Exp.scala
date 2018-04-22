package algebraic.manipulator

import algebraic.manipulator.specifiers.HeadMatch

sealed trait Exp extends Depending {
  override def dependencies: Set[String] = Set.empty
  def getFree: Set[Variable]
  def getBound: Set[Variable]

  def get(tree: Tree): TraversableOnce[Exp] = tree match {
    case Tree.Leaf => List(this)
    case Tree.Node(_) => throw new IllegalArgumentException("Illegal path")
  }

  def get[T](tree: PathTree[T]): TraversableOnce[(T, Exp)] = tree match {
    case PathTree.Empty => List.empty
    case PathTree.Leaf(l) => List(l -> this)
    case PathTree.Node(_) => throw new IllegalArgumentException("Illegal path")
  }

  def set(pairs: (Variable, Exp)*): Exp = set(pairs.toMap)
  def set(map: Map[Variable, Exp]): Exp
  def setAll(dum: Map[Variable, Variable], map: Map[Variable, Exp]): Exp
  def tree:PathTree[Variable]

  def replace(tree: Tree, func: Exp => Exp): Exp = tree match {
    case Tree.Leaf => func(this)
    case Tree.Node(_) => throw new IllegalArgumentException("Illegal path")
  }

  def replace[T](tree: PathTree[T], func: (Exp, T)=>Exp): Exp = tree match {
    case PathTree.Empty => this
    case PathTree.Leaf(a) => func(this, a)
    case PathTree.Node(_) => throw new IllegalArgumentException("Illegal path")
  }

  def matchExp(exp: Exp, headMatch: HeadMatch): Option[HeadMatch]
}

case class IntVal(v: Int) extends Exp {
  override def toString: String = v.toString

  override def getFree: Set[Variable] = Set.empty
  override def getBound: Set[Variable] = Set.empty
  override def set(map: Map[Variable, Exp]): Exp = this
  override def setAll(dum: Map[Variable, Variable], map: Map[Variable, Exp]): Exp = this
  override def tree: PathTree[Variable] = PathTree.empty

  override def matchExp(exp: Exp, headMatch: HeadMatch): Option[HeadMatch] =
    Some(headMatch).filter(_ => exp == this)
}

case class Variable(name: String) extends Exp {
  override def toString: String = name

  override def dependencies: Set[String] = Set(name)
  override def getFree: Set[Variable] = Set(this)
  override def getBound: Set[Variable] = Set.empty

  override def set(map: Map[Variable, Exp]): Exp = map.getOrElse(this, this)
  override def setAll(dum: Map[Variable, Variable], map: Map[Variable, Exp]): Exp = map.getOrElse(this, this)

  override def tree: PathTree[Variable] = PathTree.Leaf(this)

  override def matchExp(exp: Exp, headMatch: HeadMatch): Option[HeadMatch] = {
    val pars = headMatch.parameters.map(p => p._1.variable -> p._1)
    if (!pars.contains(this))
      Some(headMatch).filter(_ => this == exp)
    else {
      val v = headMatch.parameters(pars(this))
      if (v.isDefined)
        Some(headMatch).filter(_ => v.get == exp)
      else
        Some(HeadMatch(headMatch.generics, headMatch.dummies, headMatch.parameters + (pars(this) -> Some(exp))))
    }
  }
}

case class Lambda(params: List[Variable], exp: Exp) extends Exp {
  assume(params.nonEmpty)

  override def toString: String =
    (if (params.length == 1) params.mkString else params.mkString("(", ",", ")")) + s" -> $exp"

  override def dependencies: Set[String] = exp.dependencies -- params.map(_.name)
  override def getFree: Set[Variable] = exp.getFree -- params
  override def getBound: Set[Variable] = exp.getBound ++ params

  override def get(tree: Tree): TraversableOnce[Exp] = tree match {
    case Tree.Leaf => List(this)
    case Tree.Node(c) =>
      if (c.keySet == Set(0)) exp.get(c(0))
      else throw new IndexOutOfBoundsException
  }

  override def get[T](tree: PathTree[T]): TraversableOnce[(T, Exp)] = tree match {
    case PathTree.Empty => Nil
    case PathTree.Leaf(l) => List(l -> this)
    case PathTree.Node(c) =>
      if (c.keySet == Set(0)) exp.get(c(0))
      else throw new IndexOutOfBoundsException
  }

  override def set(map: Map[Variable, Exp]): Exp = Lambda(params, exp.set(map -- params))

  override def setAll(dum: Map[Variable, Variable], map: Map[Variable, Exp]): Exp =
    Lambda(params.map(dum), exp.setAll(dum, map ++ params.map(p => p -> dum.getOrElse(p, p)).toMap))

  override def tree: PathTree[Variable] = Tree.edge(0) :: exp.tree

  override def replace(tree: Tree, func: Exp => Exp): Exp = tree match {
    case Tree.Leaf => func(this)
    case Tree.Node(c) =>
      if (c.keySet == Set(0)) Lambda(params, exp.replace(c(0), func))
      else throw new IndexOutOfBoundsException
  }

  override def replace[T](tree: PathTree[T], func: (Exp, T) => Exp): Exp = tree match {
    case PathTree.Empty => this
    case PathTree.Leaf(l) => func(this, l)
    case PathTree.Node(c) =>
      if (c.keySet == Set(0)) Lambda(params, exp.replace(c(0), func))
      else throw new IndexOutOfBoundsException
  }

  override def matchExp(exp: Exp, headMatch: HeadMatch): Option[HeadMatch] =
    exp match {
      case Lambda(p, e) =>
        if(this.params.length != p.length)
          None
        else {
          val b = (this.params zip p).toMap.filterKeys(headMatch.dummies.contains).mapValues(Some(_))
          if (b.exists{case (k, v) => headMatch.dummies.getOrElse(k, Some(k)).exists(_ != v.get)})
            None
          else {
            val rem = HeadMatch(Map.empty, Map.empty, headMatch.parameters.filterKeys(d => params.contains(d.variable)))
            val add = HeadMatch(Map.empty, Map.empty, b.map{case (k, v) => Definition(AnyType, k) -> v})

            this.exp.matchExp(e, headMatch -- rem ++ add ++ HeadMatch(Map.empty, b, Map.empty)).map(_ -- add ++ rem)
          }
        }
      case _ => None
    }
}

case class Operation(op: Exp, parameters: List[Exp]) extends Exp {
  override def toString: String = op match {
    case _: Lambda => s"($op)${parameters.mkString("(",",",")")}"
    case _ => op + parameters.mkString("(",",",")")
  }

  override def dependencies: Set[String] = (op :: parameters).flatMap(_.dependencies).toSet
  override def getFree: Set[Variable] = (op :: parameters).flatMap(_.getFree).toSet
  override def getBound: Set[Variable] = (op :: parameters).flatMap(_.getBound).toSet

  override def get(tree: Tree): TraversableOnce[Exp] = tree match {
    case Tree.Leaf => List(this)
    case n @ Tree.Node(c) =>
      if (n.min < -1)
        throw new IndexOutOfBoundsException(n.min.toString)
      if (n.max >= parameters.size)
        throw new IndexOutOfBoundsException(n.max.toString)
      c.flatMap{case (k,v) => (op :: parameters)(k+1).get(v)}
  }

  override def get[T](tree: PathTree[T]): TraversableOnce[(T, Exp)] = tree match {
    case PathTree.Empty => Nil
    case PathTree.Leaf(l) => List(l -> this)
    case n @ PathTree.Node(c) =>
      if (n.min < -1)
        throw new IndexOutOfBoundsException(n.min.toString)
      if (n.max >= parameters.size)
        throw new IndexOutOfBoundsException(n.max.toString)
      c.flatMap{case (k,v) => (op :: parameters)(k+1).get(v)}
  }

  override def set(map: Map[Variable, Exp]): Exp = Operation(op.set(map), parameters.map(_.set(map)))
  override def setAll(dum: Map[Variable, Variable], map: Map[Variable, Exp]): Exp =
    Operation(op.setAll(dum, map), parameters.map(_.setAll(dum, map)))

  override def tree: PathTree[Variable] = {
    val children = ((-1 +: parameters.indices) zip (op :: parameters).map(_.tree)).toMap.filter { case (_, o) => o.nonEmpty }

    if (children.isEmpty) PathTree.empty
    else PathTree.Node(children)
  }

  override def replace(tree: Tree, func: Exp => Exp): Exp = tree match {
    case Tree.Leaf => func(this)
    case n @ Tree.Node(c) =>
      if (n.min < -1)
        throw new IndexOutOfBoundsException(n.min.toString)
      if (n.max >= parameters.size)
        throw new IndexOutOfBoundsException(n.max.toString)
      Operation(if (c.contains(-1)) op.replace(c(-1), func) else op, (parameters.indices zip parameters).map{case (i,e) => if (c.contains(i)) e.replace(c(i), func) else e}.toList)
  }

  override def replace[T](tree: PathTree[T], func: (Exp, T) => Exp): Exp = tree match {
    case PathTree.Empty => this
    case PathTree.Leaf(a) => func(this, a)
    case n @ PathTree.Node(_) =>
      if (n.min < -1)
        throw new IndexOutOfBoundsException(n.min.toString)
      if (n.max >= parameters.size)
        throw new IndexOutOfBoundsException(n.max.toString)
      Operation(op.replace(n(-1), func), (parameters.indices zip parameters).map{case (i,e) => e.replace(n(i), func)}.toList)
  }

  override def matchExp(exp: Exp, headMatch: HeadMatch): Option[HeadMatch] =
    exp match {
      case Operation(o, ps) => (Option(headMatch) /: ((op :: parameters) zip (o :: ps)))((re, e) => re.flatMap(m => e._1.matchExp(e._2, m)))
      case _ => None
    }
}
