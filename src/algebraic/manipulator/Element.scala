package algebraic.manipulator

trait Element extends Depending {
  def validate(env: Environment): Traversable[(List[String], String)]
  def filter(predicate: Element => Boolean): Traversable[Element] = Some(this).filter(predicate)
}
