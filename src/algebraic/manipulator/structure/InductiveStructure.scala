package algebraic.manipulator.structure

import algebraic.manipulator._

case class InductiveStructure(base: InductiveBase, steps: List[InductiveStep]) extends Structure {
  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(base :: steps)
}

case class InductiveBase(params: List[Definition], exp: Exp) extends Depending {
  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(params) ++ env.bind(params).dependencies(exp)
}

case class InductiveStep(v: Variable, params: List[Definition], exp: Exp) extends Depending {
  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(params) ++ env.bind(Set(v.name)).dependencies(_.bind(params).dependencies(exp))
}