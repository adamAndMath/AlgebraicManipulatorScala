package algebraic.manipulator

sealed trait Tree {
  def |(other: Tree): Tree

  def :>[T](leaf: T): PathTree[T]

  def ::(other: Tree): Tree = other match {
    case Tree.Leaf => this
    case Tree.Node(c) => Tree.Node(c.mapValues(v => v::this))
  }

  def toPaths: List[List[Int]]
}

object Tree {
  val empty: Tree = Leaf

  def from(l: Traversable[Int]): Tree = (l :\ empty)((v, t) => Node(Map(v->t)))
  def edge(edges: Int*): Tree = (edges :\ empty)((e, next) => Node(Map(e -> next)))

  case object Leaf extends Tree {
    override def |(other: Tree): Tree = other match {
      case Leaf => Leaf
      case Node(_) => throw new IllegalArgumentException
    }

    override def :>[T](leaf: T): PathTree[T] = PathTree.Leaf(leaf)

    override def toPaths: List[List[Int]] = List(Nil)

    override def toString: String = "[]"
  }

  case class Node(children: Map[Int, Tree]) extends Tree {
    assume(children.nonEmpty)

    override def |(other: Tree): Tree = other match {
      case Leaf => throw new IllegalArgumentException
      case Node(c) => Node(
        children.filterKeys(c.contains).map{case (k,v)=>(k,v|c(k))}
          ++children.filterKeys(!c.contains(_))
          ++c.filterKeys(!children.contains(_))
      )
    }

    override def :>[T](leaf: T): PathTree[T] = PathTree.Node(children map {case (k,v) => (k,v:>leaf)})

    override def toPaths: List[List[Int]] = children.toList.flatMap{case(k, t) => t.toPaths.map(k::_)}

    def min: Int = children.keys.min
    def max: Int = children.keys.max

    override def toString: String = {
      val c = children.map{ case (i, t) => t match {
        case n: Node => s"$i,$n"
        case Leaf => s"$i"
      }}

      if (children.size > 1)
        s"[${c.mkString("|")}]"
      else
        c.find(_ => true).get.toString
    }
  }

}
