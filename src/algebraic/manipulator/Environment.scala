package algebraic.manipulator

trait Environment {
  val path: Path
  def apply(path: Path): Element
  def toFull(path: Path): Path

  def dependencies(depending: Depending): Set[Path] = depending.dependencies(this)

  def dependencies(depending: Iterable[Depending]): Set[Path] =
    depending.map(_.dependencies(this)).fold(Set.empty)(_++_).filterNot(_.parent == path)

  def bind(params: List[Definition]): Environment = bind(params.map(_.name).toSet)
  def bind(bound: Set[String]): Environment = Environment.SimpleEnvironment(this, bound)
}

object Environment {
  case class SimpleEnvironment(env: Environment, bound: Set[String]) extends Environment {
    override val path: Path = env.path + ""

    override def apply(path: Path): Element = path match {
      case Path(name) if bound.contains(name) => throw new IllegalArgumentException
      case _ => env(path)
    }

    override def toFull(path: Path): Path = path match {
      case Path(name) if bound.contains(name) => this.path + name
      case _ => env.toFull(path)
    }
  }
}