package algebraic.manipulator

import scala.collection.mutable

object Path {
  val empty: Path = EmptyPath
  private class PathBuilder extends mutable.Builder[String, Path] {
    private var p: Path = Path.empty

    override def +=(elem: String): PathBuilder.this.type = {
      p += elem
      this
    }

    override def clear(): Unit = p = Path.empty

    override def result(): Path = p
  }

  def apply(elems: List[String]): Path = (empty /: elems)(_+_)
  def apply(elems: String*): Path = (empty /: elems)(_+_)
  def unapplySeq(path: Path): Option[Seq[String]] = Some(path.toList)
}

sealed abstract class Path {
  def parent: Path
  def last: String
  def isEmpty: Boolean
  def size: Int

  override def toString: String = mkString(".")
  def mkString(sep: String): String
  def toList: List[String] = {
    def to(path: Path, acc: List[String]): List[String] = path match {
      case Path.empty => acc
      case p + l => to(p, l :: acc)
    }

    to(this, Nil)
  }

  def +(elem: String): Path = new +(this, elem)

  def dropRight(i: Int): Path =
    if (i == 0) this
    else if (i < 0) throw new IndexOutOfBoundsException
    else this match {
      case EmptyPath => throw new IndexOutOfBoundsException
      case p + _ => p.dropRight(i - 1)
    }

  def common(p: Path): Path = {
    def c(a: Path, b: Path): Path = {
      val (ad, bd) = c2(a, b, _==_)
      if (ad.size == 1)
        b
      else {
        val (ap, bp) = c2(ad, bd, _!=_)
        c(ap, bp)
      }
    }

    def c2(a: Path, b: Path, predicate: (String, String) => Boolean): (Path, Path) = (a, b) match {
      case (ap+al+_, bp+bl+_) if predicate(al, bl) => c2(ap+al, bp+bl, predicate)
      case _ => (a, b)
    }

    val as = size
    val bs = p.size

    if (as == bs)
      c(this, p)
    else if (as > bs)
      c(this.dropRight(as - bs), p)
    else
      c(this, p.dropRight(bs - as))
  }

}

case object EmptyPath extends Path {
  override def parent: Path = throw new IndexOutOfBoundsException
  override def last: String = throw new IndexOutOfBoundsException
  override def isEmpty: Boolean = true
  override def size: Int = 0
  override def mkString(sep: String): String = ""
}

case class +(override val parent: Path, override val last: String) extends Path {
  override def isEmpty: Boolean = false
  override def size: Int = 1 + parent.size
  override def mkString(sep: String): String = parent match {
    case EmptyPath => last
    case _ + _ => s"$parent$sep$last"
  }
}
