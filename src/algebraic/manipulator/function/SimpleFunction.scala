package algebraic.manipulator.function

import algebraic.manipulator._

case class SimpleFunction(header: Header, exp: Exp) extends FunctionElement {
  override def paramTypes: List[Type] = header.parameters.map(_.varType)

  override def dependencies: Set[String] =
    header.scope(exp.dependencies)

  override def validate(env: Environment): Traversable[(List[String], String)] = None
}
