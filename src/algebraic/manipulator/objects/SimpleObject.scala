package algebraic.manipulator.objects

import algebraic.manipulator.{Environment, Exp, Path}

case class SimpleObject(exp: Exp) extends ObjectElement {
  override def dependencies(env: Environment): Set[Path] = env.dependencies(exp)
}
