package algebraic.manipulator.manipulation

import algebraic.manipulator._

abstract class Manipulation extends Depending {
  def apply(env: Environment, equation: List[Exp]): List[Exp]
}
