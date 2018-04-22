package algebraic.manipulator.specifiers

import algebraic.manipulator.{Depending, Type, Variable}

case class TypeHeader(generics: List[Variable], parameters: List[Type]) extends Depending {
  override def dependencies: Set[String] = parameters.flatMap(_.dependencies).toSet
}
