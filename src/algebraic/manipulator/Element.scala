package algebraic.manipulator

trait Element extends Depending {
  def validate(env: Environment): Traversable[String]
}
