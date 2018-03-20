package algebraic.manipulator.function

import algebraic.manipulator._

case class InductiveFunction(params: List[Definition], base: InductiveBase, steps: List[InductiveStep]) extends Function {
  override def paramTypes: List[Type] = params.map(_.varType)

  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(params) ++ env.bind(params).dependencies(base :: steps)
}

case class InductiveBase(inductives: Map[Variable, Exp], exp: Exp) extends Depending {
  override def dependencies(env: Environment): Set[Path] = env.dependencies(exp :: inductives.values.toList)
}

case class InductiveStep(v: Variable, params: List[Definition], exp: Exp) extends Depending {
  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(params) ++ env.bind(params).dependencies(params)
}