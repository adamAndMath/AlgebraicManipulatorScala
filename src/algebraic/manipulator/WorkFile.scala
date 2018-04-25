package algebraic.manipulator

class WorkFile(parent: Environment, name: String, val using: Map[String, List[String]], val imports: Set[Environment]) extends Environment {
  override val path: List[String] = parent.path :+ name

  override def find(path: List[String], predicate: Element => Boolean): Option[Element] =
    using.get(path.head).flatMap(parent.find(_, _ => true)).flatMap{
      case env: Environment if path.tail.nonEmpty => env(path.tail, predicate)
      case e => Some(e).filter(predicate)
    }.orElse((Option.empty[Element] /: imports)(_ orElse _(path, predicate)))
      .orElse(parent.find(path, predicate))

  override def toFull(path: List[String], predicate: Element => Boolean): Option[List[String]] =
    using.get(path.head).flatMap(parent.find(_, _ => true)).flatMap{
      case env: Environment if path.tail.nonEmpty => env(path.tail, predicate).map(_ => env.path ++ path.tail)
      case e => Some(e).filter(predicate).flatMap(_ => parent.toFull(using(path.head), predicate))
    }.orElse(imports.find(i => i.contains(path.head) && i(path, predicate).isDefined).map(_.path ++ path))
      .orElse(parent.toFull(path, predicate))
}
