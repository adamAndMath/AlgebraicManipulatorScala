package algebraic.manipulator.function

import algebraic.manipulator.specifiers.TypeHeader
import algebraic.manipulator.Environment

case class AssumedFunction(header: TypeHeader) extends FunctionElement {
  override def typeHeader: TypeHeader = header

  override def dependencies: Set[String] = header.dependencies

  override def validate(env: Environment): Traversable[(List[String], String)] = None
}
