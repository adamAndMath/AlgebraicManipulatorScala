package algebraic.manipulator

import algebraic.manipulator.specifiers.Header

case class Property(header: Header, ideHeader: Header, ide: List[Exp]) extends Element {
  override def validate(env: Environment): Traversable[(List[String], String)] = None

  override def dependencies: Set[String] =
    header.scope(ideHeader.scope(ide.flatMap(_.dependencies).toSet))
}
