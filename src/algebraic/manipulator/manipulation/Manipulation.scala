package algebraic.manipulator.manipulation

import algebraic.manipulator._

abstract class Manipulation extends Depending {
  def apply(finder: Project.Finder, equation: List[Exp]): List[Exp]
  override def dependencies(finder: Project.Finder): Set[Path] = Set.empty
}
