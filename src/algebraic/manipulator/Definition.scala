package algebraic.manipulator

case class Definition(varType: Type, name: String) extends Depending {
  def variable: Variable = Variable(name)

  override def dependencies(finder: Project.Finder): Set[Path] = varType.dependencies(finder)

  override def toString: String = s"$varType $name"
}
