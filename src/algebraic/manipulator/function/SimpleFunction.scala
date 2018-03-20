package algebraic.manipulator.function

import algebraic.manipulator._

case class SimpleFunction(params: List[Definition], exp: Exp) extends Function {
  override def paramTypes: List[Type] = params.map(_.varType)

  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(params) ++ env.bind(params).dependencies(exp)
}
