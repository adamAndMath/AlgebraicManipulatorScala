package algebraic.manipulator.function

import algebraic.manipulator._

case class InductiveFunction(header: Header, base: InductiveBase, steps: List[InductiveStep]) extends FunctionElement {
  override def paramTypes: List[Type] = header.parameters.map(_.varType)

  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(header) ++ header.bind(env).dependencies(base :: steps)

  override def validate(env: Environment): Traversable[String] = None
}

case class InductiveBase(inductive: Variable, value: Exp, exp: Exp) extends Depending {
  override def dependencies(env: Environment): Set[Path] = env.dependencies(List(value, exp))
}

case class InductiveStep(params: List[Definition], step: Exp, exp: Exp) extends Depending {
  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(params) ++ env.bind(params).dependencies(List(step, exp))
}