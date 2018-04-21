package algebraic.manipulator.structure

import algebraic.manipulator._

case class InductiveStructure(base: InductiveBase, steps: List[InductiveStep]) extends Structure {
  override def dependencies: Set[String] =
    (base :: steps).flatMap(_.dependencies).toSet

  override def validate(env: Environment): Traversable[(List[String], String)] = None
}

case class InductiveBase(params: List[Definition], exp: Exp) extends Depending {
  override def dependencies: Set[String] =
    Header(Nil, params).scope(exp.dependencies)
}

case class InductiveStep(v: Variable, params: List[Definition], exp: Exp) extends Depending {
  override def dependencies: Set[String] =
    Header(Nil, params).scope(exp.dependencies - v.name)
}