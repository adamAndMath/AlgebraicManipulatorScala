package algebraic.manipulator.structure

import algebraic.manipulator.{Path, Project}

object SimpleStructure extends Structure {
  override def dependencies(finder: Project.Finder): Set[Path] = Set.empty
}
