package algebraic.manipulator

trait Depending {
  def dependencies(finder: Project.Finder): Set[Path]
}
