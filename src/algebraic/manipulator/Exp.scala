package algebraic.manipulator

sealed abstract class Exp {
  def getFree: Set[Variable] = Set.empty
  def getBound: Set[Variable] = Set.empty

  def get(tree: Tree): TraversableOnce[Exp] = tree match {
    case Tree.Leaf => List(this)
    case Tree.Node(_) => throw new IllegalArgumentException
  }

  def get[T](tree: PathTree[T]): TraversableOnce[(Exp, T)] = tree match {
    case PathTree.Leaf(l) => List((this, l))
    case PathTree.Node(_) => throw new IllegalArgumentException
  }

  def set(map: Variable=>Exp): Exp = this
  def setAll(dum: Map[Variable, Variable], map: Variable => Exp): Exp = this
  def tree[T](toLeaf: Variable=>T):PathTree[T] = PathTree.empty

  def replace(tree: Tree, func: Exp=>Exp): Exp = tree match {
    case Tree.Leaf => func(this)
    case Tree.Node(_) => throw new IllegalArgumentException
  }

  def replace[T](tree: PathTree[T], func: (Exp, T)=>Exp): Exp = tree match {
    case PathTree.Leaf(a) => func(this, a)
    case PathTree.Node(_) => throw new IllegalArgumentException
  }
}

case class IntVal(v: Int) extends Exp {
  override def toString: String = v.toString
}

case class Constant(name: String) extends Exp {
  override def toString: String = s"\\$name"
}

case class Variable(name: String) extends Exp {
  override def toString: String = name

  override def getFree: Set[Variable] = Set(this)

  override def set(map: Variable => Exp): Exp = map(this)
  override def setAll(dum: Map[Variable, Variable], map: Variable => Exp): Exp = dum.applyOrElse(this, map)

  override def tree[T](toLeaf: Variable => T): PathTree[T] = PathTree.Leaf(toLeaf(this))
}

case class Operation(name: String, dummies: List[Variable], parameters: List[Exp]) extends Exp {
  override def toString: String = name + {if (dummies.isEmpty) "" else dummies.mkString("<", ",", ">")} + parameters.mkString("(",",",")")

  override def get(tree: Tree): TraversableOnce[Exp] = tree match {
    case Tree.Leaf => List(this)
    case n @ Tree.Node(c) =>
      if (n.min < 0 || n.max > parameters.size)
        throw new IllegalArgumentException
      else
        c.flatMap{case (k,v) => parameters(k).get(v)}
  }

  override def get[T](tree: PathTree[T]): TraversableOnce[(Exp, T)] = tree match {
    case PathTree.Leaf(l) => List((this, l))
    case n @ PathTree.Node(c) =>
      if (n.min < 0 || n.max > parameters.size)
        throw new IllegalArgumentException
      else
        c.flatMap{case (k,v) => parameters(k).get(v)}
  }

  override def set(map: Variable => Exp): Exp = Operation(name, dummies, parameters.map(_.set(map)))
  override def setAll(dum: Map[Variable, Variable], map: Variable => Exp): Exp =
    Operation(name, dummies.map(v => dum.getOrElse(v, v)), parameters.map(_.setAll(dum, map)))

  override def tree[T](toLeaf: Variable => T): PathTree[T] = {
    val children = (parameters.indices zip parameters.map(_.tree(toLeaf))).toMap.filter { case (_, o) => o.nonEmpty }

    if (children.isEmpty) PathTree.empty
    else PathTree.Node(children)
  }

  override def replace(tree: Tree, func: Exp => Exp): Exp = tree match {
    case Tree.Leaf => func(this)
    case n @ Tree.Node(c) =>
      if (n.min < 0 || n.max > parameters.size)
        throw new IllegalArgumentException
      else
        Operation(name, dummies, (parameters.indices zip parameters).map{case (i,e) => if (c.contains(i)) e.replace(c(i), func) else e}.toList)
  }

  override def replace[T](tree: PathTree[T], func: (Exp, T) => Exp): Exp = tree match {
    case PathTree.Leaf(a) => func(this, a)
    case n @ PathTree.Node(c) =>
      if (n.min < 0 || n.max > parameters.size)
        throw new IllegalArgumentException
      else
        Operation(name, dummies, (parameters.indices zip parameters).map{case (i,e) => if (c.contains(i)) e.replace(c(i), func) else e}.toList)
  }
}