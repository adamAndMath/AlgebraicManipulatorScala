package algebraic.manipulator

case class Definition(varType: Type, name: String) extends Depending {
  def variable: Variable = Variable(name)

  override def dependencies(env: Environment): Set[Path] = env.dependencies(varType)

  override def toString: String = s"$varType $name"
}
