package algebraic.manipulator

sealed trait Type extends Depending

case class SimpleType(name: String) extends Type {
  override def dependencies: Set[String] = Set(name)
  override def toString: String = name
}

case class TupleType(types: List[Type]) extends Type {
  override def dependencies: Set[String] = types.flatMap(_.dependencies).toSet
  override def toString: String = s"(${types.mkString(",")})"
  assume(types.length != 1)
}

case class FuncType(from: Type, to: Type) extends Type {
  override def dependencies: Set[String] = from.dependencies ++ to.dependencies
  override def toString: String = s"$from -> $to"
}
