package algebraic.manipulator.function

import algebraic.manipulator._

case class InductiveFunction(header: Header, base: InductiveBase, steps: List[InductiveStep]) extends FunctionElement {
  override def paramTypes: List[Type] = header.parameters.map(_.varType)

  override def dependencies: Set[String] =
    header.scope((base :: steps).flatMap(_.dependencies).toSet)

  override def validate(env: Environment): Traversable[(List[String], String)] = None
}

case class InductiveBase(inductive: Variable, value: Exp, exp: Exp) extends Depending {
  override def dependencies: Set[String] = value.dependencies ++ exp.dependencies
}

case class InductiveStep(params: List[Definition], step: Exp, exp: Exp) extends Depending {
  override def dependencies: Set[String] =
    Header(Nil, params).scope(step.dependencies ++ exp.dependencies)
}