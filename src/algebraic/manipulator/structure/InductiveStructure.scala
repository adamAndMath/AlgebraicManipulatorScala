package algebraic.manipulator.structure

import algebraic.manipulator._

case class InductiveStructure(base: InductiveBase, steps: List[InductiveStep]) extends Structure {
  override def dependencies(env: Environment): Set[Path] = (base :: steps).map(_.dependencies(env)).fold(Set.empty)(_++_)
}

case class InductiveBase(params: List[Definition], exp: Exp) extends Depending {
  override def dependencies(env: Environment): Set[Path] = params.map(_.dependencies(env)).fold(Set.empty)(_++_)
}

case class InductiveStep(v: Variable, params: List[Definition], exp: Exp) extends Depending {
  override def dependencies(env: Environment): Set[Path] = params.map(_.dependencies(env)).fold(Set.empty)(_++_)
}