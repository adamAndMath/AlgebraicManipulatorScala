package algebraic.manipulator.function

import algebraic.manipulator.{Environment, Path, Type}

case class AssumedFunction(params: List[Type]) extends FunctionElement {
  override def paramTypes: List[Type] = params

  override def dependencies(env: Environment): Set[Path] = env.dependencies(params)
}
