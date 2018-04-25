package algebraic.manipulator.structure

import algebraic.manipulator.Environment

object SimpleStructure extends Structure {
  override def validate(env: Environment): Traversable[(List[String], String)] = None
}
