package algebraic.manipulator

class WorkFile(parent: Environment, name: String, val using: Map[String, List[String]], val imports: Set[Environment]) extends Environment {
  override val path: List[String] = parent.path :+ name

  override def find[E <: Element](path: List[String], predicate: E => Boolean): Option[E] =
    using.get(path.head).flatMap(parent.find[Element](_)).flatMap{
      case env: Environment if path.tail.nonEmpty => env(path.tail, predicate)
      case e: E => Some(e).filter(predicate)
      case _ => None
    }.orElse((Option.empty[E] /: imports)(_ orElse _(path, predicate)))
      .orElse(parent.find(path, predicate))

  override def toFull[E <: Element](path: List[String], predicate: E => Boolean): Option[List[String]] =
    using.get(path.head).flatMap(parent.find[Element](_)).flatMap{
      case env: Environment if path.tail.nonEmpty => env(path.tail, predicate).map(_ => env.path ++ path.tail)
      case e: E => Some(e).filter(predicate).flatMap(_ => parent.toFull(using(path.head)))
      case _ => None
    }.orElse(imports.find(i => i.contains(path.head) && i(path, predicate).isDefined).map(_.path ++ path))
      .orElse(parent.toFull(path, predicate))
}
