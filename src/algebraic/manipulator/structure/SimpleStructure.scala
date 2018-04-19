package algebraic.manipulator.structure

import algebraic.manipulator.{Environment, Path}

object SimpleStructure extends Structure {
  override def dependencies(env: Environment): Set[Path] = Set.empty

  override def validate(env: Environment): Traversable[String] = None
}
