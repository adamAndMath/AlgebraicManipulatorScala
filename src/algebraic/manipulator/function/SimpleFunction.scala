package algebraic.manipulator.function

import algebraic.manipulator._
import algebraic.manipulator.specifiers.{Header, TypeHeader}

case class SimpleFunction(header: Header, exp: Exp) extends FunctionElement {
  assert(header.dummies.toSet == exp.getBound, s"${header.dummies} doesn't match ${exp.getBound}")

  override def typeHeader: TypeHeader = header.toType

  override def dependencies: Set[String] =
    header.scope(exp.dependencies)

  override def validate(env: Environment): Traversable[(List[String], String)] = None
}
