package algebraic.manipulator.objects

import algebraic.manipulator.{Environment, Exp}

case class SimpleObject(exp: Exp) extends ObjectElement {
  override def dependencies: Set[String] = exp.dependencies

  override def validate(env: Environment): Traversable[(List[String], String)] = None
}
