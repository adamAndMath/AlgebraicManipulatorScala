package algebraic.manipulator.objects

import algebraic.manipulator.manipulation.Wrapable
import algebraic.manipulator.{Environment, Exp, Variable}

case class SimpleObject(value: Exp) extends ObjectElement with Wrapable {
  override def dependencies: Set[String] = value.dependencies

  override def validate(env: Environment): Traversable[(List[String], String)] = None

  override def wrap(template: Exp, exp: Exp): Exp = template match {
    case Variable(_) =>
      if (value != exp)
        throw new IllegalArgumentException(s"Expected $value, but received $exp")
      template
    case _ => throw new IllegalArgumentException(s"Invalid template $template")
  }

  override def unwrap(exp: Exp): Exp = exp match {
    case Variable(_) => value
    case _ => throw new IllegalArgumentException(s"Unable to unwrap $exp")
  }
}
