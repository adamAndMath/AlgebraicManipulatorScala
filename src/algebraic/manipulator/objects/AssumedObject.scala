package algebraic.manipulator.objects
import algebraic.manipulator.Environment

object AssumedObject extends ObjectElement {
  override def validate(name: String, env: Environment): Traversable[(List[String], String)] = None
}
