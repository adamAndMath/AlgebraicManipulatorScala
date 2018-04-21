package algebraic.manipulator.function

import algebraic.manipulator.{Environment, Type}

case class AssumedFunction(params: List[Type]) extends FunctionElement {
  override def paramTypes: List[Type] = params

  override def dependencies: Set[String] = params.flatMap(_.dependencies).toSet

  override def validate(env: Environment): Traversable[(List[String], String)] = None
}
