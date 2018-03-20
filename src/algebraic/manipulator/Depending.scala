package algebraic.manipulator

trait Depending {
  def dependencies(env: Environment): Set[Path]
}
