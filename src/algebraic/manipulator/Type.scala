package algebraic.manipulator

sealed trait Type

case class SimpleType(name: String) extends Type {
  override def toString: String = name
}

case class TupleType(types: List[Type]) extends Type {
  override def toString: String = s"(${types.mkString(",")})"
  assume(types.length != 1)
}

case class FuncType(from: Type, to: Type) extends Type {
  override def toString: String = s"$from -> $to"
}
