package algebraic.manipulator

case class Definition(varType: Type, name: String) extends Depending {
  def variable: Variable = Variable(name)

  override def dependencies: Set[String] = varType.dependencies

  override def toString: String = s"$varType $name"
}
