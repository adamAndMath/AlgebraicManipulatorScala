package algebraic.manipulator

class WorkFile(parent: Environment, name: String, val using: Map[String, List[String]], val imports: Set[Environment]) extends Environment {
  override val path: List[String] = parent.path :+ name
  override val bound: Set[String] = imports.flatMap(_.names) ++ using.keys

  override def dependencies: Set[String] =
    imports.map(_.path).map(p => p.drop((p zip path).takeWhile(e => e._1==e._2).length).head) ++ using.values.map(_.head)

  override def find[E <: Element](path: List[String], predicate: E => Boolean): Option[E] =
    using.get(path.head).flatMap(parent.find[Element](_)).flatMap{
      case env: Environment if path.tail.nonEmpty => env(path.tail, predicate)
      case e => Some(e).filter(_.isInstanceOf[E]).map(_.asInstanceOf[E]).filter(predicate)
    }.orElse((Option.empty[E] /: imports)(_ orElse _(path, predicate)))
      .orElse(parent.find(path, predicate))

  override def toFull[E <: Element](path: List[String], predicate: E => Boolean): Option[List[String]] =
    using.get(path.head).flatMap(parent.find[Element](_)).flatMap{
      case env: Environment if path.tail.nonEmpty => env(path.tail, predicate).map(_ => env.path ++ path.tail)
      case e => Some(e).filter(_.isInstanceOf[E]).map(_.asInstanceOf[E]).filter(predicate).flatMap(_ => parent.toFull(using(path.head)))
    }.orElse(imports.find(i => i.contains(path.head) && i(path, predicate).isDefined).map(_.path ++ path))
      .orElse(parent.toFull(path, predicate))
}
