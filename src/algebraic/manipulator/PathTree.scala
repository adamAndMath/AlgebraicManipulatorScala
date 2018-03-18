package algebraic.manipulator

sealed abstract class PathTree[+T] {
  val isLeaf: Boolean = false
  val isEmpty: Boolean = false
  def nonEmpty: Boolean = !isEmpty
  def apply(i: Int): PathTree[T] = PathTree.empty
  def ::(other: Tree): PathTree[T] = other match {
    case Tree.Leaf => this
    case Tree.Node(c) => PathTree.Node(c map {case (k,v) => (k,v::this)})
  }

  def |[B >: T](other: PathTree[B]): PathTree[B]

  def filter(predicate: T => Boolean): PathTree[T]
  def map[U](map: T => U): PathTree[U]
}

object PathTree {
  def leaf[T](leaf: T): PathTree[T] = Leaf(leaf)
  def empty[T]: PathTree[T] = Empty

  case object Empty extends PathTree[Nothing] {
    override val isEmpty: Boolean = true
    override def ::(other: Tree): PathTree[Nothing] = Empty
    override def |[B](other: PathTree[B]): PathTree[B] = other
    override def filter(predicate: Nothing => Boolean): PathTree[Nothing] = Empty
    override def map[U](map: Nothing => U): PathTree[U] = Empty
  }

  case class Leaf[T](leaf: T) extends PathTree[T] {
    override val isLeaf: Boolean = true
    override def |[B >: T](other: PathTree[B]): PathTree[B] = other match {
      case Empty => this
      case Leaf(l) => if (leaf.equals(l)) Leaf(leaf) else throw new IllegalArgumentException
      case Node(_) => throw new IllegalArgumentException
    }

    override def filter(predicate: T => Boolean): PathTree[T] = if (predicate(leaf)) this else Empty

    override def map[U](map: T => U): PathTree[U] = Leaf(map(leaf))

    override def toString: String = s"($leaf)"
  }

  case class Node[T](children: Map[Int, PathTree[T]]) extends PathTree[T] {
    assume(children.nonEmpty)
    assume(children.forall(_._2.nonEmpty))

    override def apply(i: Int): PathTree[T] = children.getOrElse(i, empty)

    override def |[B >: T](other: PathTree[B]): PathTree[B] = other match {
      case Empty => this
      case Leaf(_) => throw new IllegalArgumentException
      case Node(c) => Node(
        children.filterKeys(c.contains).map{case (k,v)=>(k,v|c(k))}
          .++(children.filterKeys(!c.contains(_))
          ++c.filterKeys(!children.contains(_)))
      )
    }

    override def filter(predicate: T => Boolean): PathTree[T] = {
      val c = children.mapValues(_.filter(predicate)).filter{_._2.nonEmpty}

      if (c.isEmpty) Empty else Node(c)
    }

    override def map[U](map: T => U): PathTree[U] = Node(children.mapValues(_.map(map)))

    def min(): Int = children.keys.min
    def max(): Int = children.keys.max

    override def toString: String = {
      val c = children.map{ case (i, t) => s"$i,$t" }

      if (children.size > 1)
        s"[${c.mkString("|")}]"
      else
        c.find(_ => true).get.toString
    }
  }
}
