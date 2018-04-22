package algebraic.manipulator

import algebraic.manipulator.specifiers.Header

case class Property(header: Header, assumption: Assumption) extends Element {
  override def validate(env: Environment): Traversable[(List[String], String)] =
    assumption.validate(header.bind(env))

  override def dependencies: Set[String] =
    header.scope(assumption.dependencies)
}
