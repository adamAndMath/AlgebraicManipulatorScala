package algebraic.manipulator

case class Definition(varType: Type, variable: Variable) extends Depending {
  def name: String = variable.name

  override def dependencies: Set[String] = varType.dependencies

  override def toString: String = s"$varType $name"
}
