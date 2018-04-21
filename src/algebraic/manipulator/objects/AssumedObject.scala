package algebraic.manipulator.objects
import algebraic.manipulator.Environment

object AssumedObject extends ObjectElement {
  override def dependencies: Set[String] = Set.empty

  override def validate(env: Environment): Traversable[(List[String], String)] = None
}
