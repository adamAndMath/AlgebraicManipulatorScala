package algebraic.manipulator.manipulation

import algebraic.manipulator._

case class Call(temp: Variable, exp: Exp) extends Manipulation {
  override def apply(env: Environment, equation: List[Exp]): List[Exp] =
    equation.map(e => exp.set(temp -> e))

  override def dependencies: Set[String] = exp.dependencies - temp.name
}
