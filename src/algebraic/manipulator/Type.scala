package algebraic.manipulator

sealed trait Type extends Depending

case object AnyType extends Type {
  override def dependencies: Set[String] = Set.empty
}

case class SimpleType(name: String, gen: List[Type]) extends Type {
  override def dependencies: Set[String] = gen.flatMap(_.dependencies).toSet + name
  override def toString: String = if (gen.isEmpty) name else s"$name[${gen.mkString(",")}]"
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
