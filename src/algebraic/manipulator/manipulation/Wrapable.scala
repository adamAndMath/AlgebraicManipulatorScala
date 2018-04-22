package algebraic.manipulator.manipulation

import algebraic.manipulator.{Element, Exp}

trait Wrapable extends Element {
  def wrap(template: Exp, exp: Exp): Exp
  def unwrap(exp: Exp): Exp
}
