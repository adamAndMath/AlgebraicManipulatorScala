package algebraic.manipulator.manipulation

import algebraic.manipulator._

abstract class Manipulation {
  def apply(finder: Project.Finder, equation: List[Exp]): List[Exp]
  def dependencies(finder: Project.Finder): Set[List[String]] = Set.empty
}


