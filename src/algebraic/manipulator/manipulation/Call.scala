package algebraic.manipulator.manipulation

import algebraic.manipulator._

case class Call(temp: Variable, exp: Exp) extends Manipulation {
  override def apply(finder: Project.Finder, equation: List[Exp]): List[Exp] =
    equation.map(e => exp.set(v => if (v == temp) e else v))
}
