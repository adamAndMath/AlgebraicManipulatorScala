package algebraic.manipulator.objects
import algebraic.manipulator.{Environment, Path}

object AssumedObject extends ObjectElement {
  override def dependencies(env: Environment): Set[Path] = Set.empty

  override def validate(env: Environment): Traversable[String] = None
}
