package algebraic.manipulator.function

import algebraic.manipulator.{Depending, Type}

trait Function extends Depending {
  def paramTypes: List[Type]
}
