package algebraic.manipulator.structure

import algebraic.manipulator._
import algebraic.manipulator.specifiers.Header

case class InductiveStructure(header: Header, base: InductiveBase, steps: List[InductiveStep]) extends Structure {
  override def validate(env: Environment): Traversable[(List[String], String)] = None
}

case class InductiveBase(params: List[Definition], exp: Exp) extends Depending {
  override def dependencies: Set[String] =
    Header(Nil, Nil, params).scope(exp.dependencies)
}

case class InductiveStep(v: Variable, params: List[Definition], exp: Exp) extends Depending {
  override def dependencies: Set[String] =
    Header(Nil, Nil, params).scope(exp.dependencies - v.name)
}