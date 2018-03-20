package algebraic.manipulator.read

import algebraic.manipulator.{Depending, Element, Environment}

trait ElementTemplate extends Depending {
  def apply(env: Environment): Element
}
