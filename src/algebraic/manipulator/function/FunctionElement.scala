package algebraic.manipulator.function

import algebraic.manipulator.{Element, Type}

trait FunctionElement extends Element {
  def paramTypes: List[Type]
}
