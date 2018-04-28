package algebraic.manipulator.manipulation

import algebraic.manipulator.{Element, Environment, Exp}
import algebraic.manipulator.specifiers.Specifier

trait Substitutable extends Element {
  def substitute(env: Environment, exp: Exp, specifiers: List[Specifier], from: Int, to: Int): Exp
}
