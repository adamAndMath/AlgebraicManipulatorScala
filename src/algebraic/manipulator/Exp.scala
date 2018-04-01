package algebraic.manipulator

sealed trait Exp extends Depending {
  override def dependencies(env: Environment): Set[Path] = Set.empty
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

  def matchExp(exp: Exp, params: Map[Variable, Option[Exp]]): Option[Map[Variable, Option[Exp]]] = {
    val parTree = tree.filter(!getBound.contains(_))
    var pars = params

    if (parTree.nonEmpty) {
      for ((p, e) <- exp.get(parTree)) {
        if (!pars.contains(p)) {
          if (p != e)
            return None
        } else if (pars(p).isEmpty)
          pars += (p -> Some(e))
        else if (!pars(p).contains(e))
          return None
      }
    }

    Some(pars)
  }
}

case class IntVal(v: Int) extends Exp {
  override def toString: String = v.toString

  override def getFree: Set[Variable] = Set.empty
  override def getBound: Set[Variable] = Set.empty
  override def set(map: Map[Variable, Exp]): Exp = this
  override def setAll(dum: Map[Variable, Variable], map: Map[Variable, Exp]): Exp = this
  override def tree: PathTree[Variable] = PathTree.empty
}

case class Variable(name: String) extends Exp {
  override def toString: String = name

  override def dependencies(env: Environment): Set[Path] = Set(env.toFull(Path(name)))
  override def getFree: Set[Variable] = Set(this)
  override def getBound: Set[Variable] = Set.empty

  override def set(map: Map[Variable, Exp]): Exp = map.getOrElse(this, this)
  override def setAll(dum: Map[Variable, Variable], map: Map[Variable, Exp]): Exp = dum.getOrElse(this, map.getOrElse(this, this))

  override def tree: PathTree[Variable] = PathTree.Leaf(this)
}

case class Lambda(params: List[Variable], exp: Exp) extends Exp {
  assume(params.nonEmpty)

  override def toString: String =
    (if (params.length == 1) params.mkString else params.mkString("(", ",", ")")) + s" -> $exp"

  override def dependencies(env: Environment): Set[Path] = env.bind(params.map(_.toString).toSet).dependencies(exp)
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
}

case class Operation(op: Exp, parameters: List[Exp]) extends Exp {
  override def toString: String = op match {
    case _: Lambda => s"($op)${parameters.mkString("(",",",")")}"
    case _ => op + parameters.mkString("(",",",")")
  }

  override def dependencies(env: Environment): Set[Path] = env.dependencies(op :: parameters)
  override def getFree: Set[Variable] = (op :: parameters).map(_.getFree).fold(Set.empty)(_++_)
  override def getBound: Set[Variable] = (op :: parameters).map(_.getBound).fold(Set.empty)(_++_)

  override def get(tree: Tree): TraversableOnce[Exp] = tree match {
    case Tree.Leaf => List(this)
    case n @ Tree.Node(c) =>
      if (n.min < 0 || n.max >= parameters.size)
        throw new IllegalArgumentException
      else
        c.flatMap{case (k,v) => parameters(k).get(v)}
  }

  override def get[T](tree: PathTree[T]): TraversableOnce[(T, Exp)] = tree match {
    case PathTree.Empty => Nil
    case PathTree.Leaf(l) => List(l -> this)
    case n @ PathTree.Node(c) =>
      if (n.min < -1 || n.max >= parameters.size)
        throw new IndexOutOfBoundsException
      else
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
      if (n.min < -1 || n.max >= parameters.size)
        throw new IllegalArgumentException
      else
        Operation(if (c.contains(-1)) op.replace(c(-1), func) else op, (parameters.indices zip parameters).map{case (i,e) => if (c.contains(i)) e.replace(c(i), func) else e}.toList)
  }

  override def replace[T](tree: PathTree[T], func: (Exp, T) => Exp): Exp = tree match {
    case PathTree.Empty => this
    case PathTree.Leaf(a) => func(this, a)
    case n @ PathTree.Node(_) =>
      if (n.min < -1 || n.max >= parameters.size)
        throw new IllegalArgumentException
      else
        Operation(op.replace(n(-1), func), (parameters.indices zip parameters).map{case (i,e) => e.replace(n(i), func)}.toList)
  }
}
