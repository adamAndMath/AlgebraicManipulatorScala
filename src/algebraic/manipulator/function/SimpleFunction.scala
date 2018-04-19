package algebraic.manipulator.function

import algebraic.manipulator._

case class SimpleFunction(header: Header, exp: Exp) extends FunctionElement {
  override def paramTypes: List[Type] = header.parameters.map(_.varType)

  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(header) ++ header.bind(env).dependencies(exp)

  override def validate(env: Environment): Traversable[String] = None
}
