package algebraic.manipulator.manipulation

import algebraic.manipulator.{Element, Exp}
import algebraic.manipulator.specifiers.{HeadMatch, Specifier}

trait Substitutable extends Element {
  def substitute(specifiers: List[Specifier]): (List[Exp], HeadMatch)
}
