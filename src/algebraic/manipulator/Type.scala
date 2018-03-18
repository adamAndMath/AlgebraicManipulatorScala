package algebraic.manipulator

sealed trait Type extends Depending

case class SimpleType(name: String) extends Type {
  override def dependencies(finder: Project.Finder): Set[Path] = Set(finder.toFull(Path(name)))
  override def toString: String = name
}

case class TupleType(types: List[Type]) extends Type {
  override def dependencies(finder: Project.Finder): Set[Path] = types.map(_.dependencies(finder)).fold(Set.empty)(_++_)
  override def toString: String = s"(${types.mkString(",")})"
  assume(types.length != 1)
}

case class FuncType(from: Type, to: Type) extends Type {
  override def dependencies(finder: Project.Finder): Set[Path] = from.dependencies(finder) ++ to.dependencies(finder)
  override def toString: String = s"$from -> $to"
}
