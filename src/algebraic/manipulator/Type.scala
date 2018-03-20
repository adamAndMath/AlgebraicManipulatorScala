package algebraic.manipulator

sealed trait Type extends Depending

case class SimpleType(name: String) extends Type {
  override def dependencies(env: Environment): Set[Path] = Set(env.toFull(Path(name)))
  override def toString: String = name
}

case class TupleType(types: List[Type]) extends Type {
  override def dependencies(env: Environment): Set[Path] = env.dependencies(types)
  override def toString: String = s"(${types.mkString(",")})"
  assume(types.length != 1)
}

case class FuncType(from: Type, to: Type) extends Type {
  override def dependencies(env: Environment): Set[Path] = env.dependencies(List(from, to))
  override def toString: String = s"$from -> $to"
}
