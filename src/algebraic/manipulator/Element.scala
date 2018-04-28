package algebraic.manipulator

trait Element {
  def validate(name: String, env: Environment): Traversable[(List[String], String)]
  def filter(predicate: Element => Boolean): Traversable[Element] = Some(this).filter(predicate)
}
