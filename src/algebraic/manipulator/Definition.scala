package algebraic.manipulator

case class Definition(varType: Type, name: String) {
  def variable(): Variable = Variable(name)

  override def toString: String = s"$varType $name"
}
