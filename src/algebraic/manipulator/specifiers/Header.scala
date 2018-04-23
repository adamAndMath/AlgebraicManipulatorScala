package algebraic.manipulator.specifiers

import algebraic.manipulator.structure.SimpleStructure
import algebraic.manipulator.{Definition, Depending, Environment, Variable}

case class Header(generics: List[Variable], dummies: List[Variable], parameters: List[Definition]) extends Depending {
  override def dependencies: Set[String] = parameters.flatMap(_.dependencies).toSet -- generics.map(_.name)

  override def toString: String =
    if (dummies.isEmpty) s"(${parameters.mkString(",")})"
    else s"<${dummies.mkString(",")}>(${parameters.mkString(",")})"

  def scope(deps: Set[String]): Set[String] =
    deps -- parameters.map(_.name) -- generics.map(_.name) ++ dependencies
  def scopeWithDummies(deps: Set[String]): Set[String] =
    deps -- parameters.map(_.name) -- dummies.map(_.name) -- generics.map(_.name) ++ dependencies

  def bind(env: Environment): Environment =
    env ++ generics.map(_.name -> SimpleStructure).toMap ++ parameters
  def bindWithDummies(env: Environment):Environment =
    env ++ generics.map(_.name -> SimpleStructure).toMap ++ parameters ++ dummies.map(_.name).toSet

  def mapPars(map: List[Definition] => List[Definition]): Header =
    Header(generics, dummies, map(parameters))

  def toType: TypeHeader = TypeHeader(generics, parameters.map(_.varType))
  def toMatch: HeadMatch = HeadMatch(generics.map(_ -> None).toMap, dummies.map(_ -> None).toMap, parameters.map(_ -> None).toMap)
}
