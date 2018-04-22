package algebraic.manipulator.function

import algebraic.manipulator._
import algebraic.manipulator.manipulation.Wrapable
import algebraic.manipulator.specifiers.{Header, TypeHeader}

case class SimpleFunction(header: Header, value: Exp) extends FunctionElement with Wrapable {
  assert(header.dummies.toSet == value.getBound, s"${header.dummies} doesn't match ${value.getBound}")

  override def typeHeader: TypeHeader = header.toType

  override def dependencies: Set[String] =
    header.scope(value.dependencies)

  override def validate(env: Environment): Traversable[(List[String], String)] = None

  override def wrap(template: Exp, exp: Exp): Exp = template match {
    case Operation(Variable(_), parameters) =>
      if (header.parameters.length != parameters.length)
        throw new IllegalArgumentException(s"Expected ${header.parameters.length} arguments, but received ${parameters.length}")
      val parMap = header.parameters.map(_.variable).zip(parameters).toMap
      val expect = value.set(parMap)
      if (expect != exp)
        throw new IllegalArgumentException(s"Expected $expect, but received $exp")
      template
    case Variable(f) =>
      try {
        val re = value.matchExp(exp, header.toMatch)
          .getOrElse(throw new IllegalStateException(s"Expected substitute of $value, but received $exp"))
        val pars = re.parameters.map{ case (p, o) => p -> o.getOrElse(throw new IllegalStateException(s"Undefined parameter $p in $exp"))}

        Operation(Variable(f), header.parameters.map(pars))
      } catch {
        case e: Exception => throw new IllegalArgumentException(s"Expected substitute of $value, but received $exp", e)
      }
    case _ => throw new IllegalArgumentException(s"Invalid expression for wrapping $template")
  }

  override def unwrap(exp: Exp): Exp = exp match {
    case Operation(Variable(_), parameters) =>
      if (header.parameters.length != parameters.length)
        throw new IllegalArgumentException(s"Expected ${header.parameters.length} arguments, but received ${parameters.length}")

      val parMap = header.parameters.map(_.variable).zip(parameters).toMap
      value.set(parMap)
    case _ => throw new IllegalArgumentException(s"Functions should only be unwrapped with parameters")
  }
}
