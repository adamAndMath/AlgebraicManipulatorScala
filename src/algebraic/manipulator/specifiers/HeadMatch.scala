package algebraic.manipulator.specifiers

import algebraic.manipulator.{Definition, Exp, Variable}

case class HeadMatch(generics: Map[Variable, Option[Variable]], dummies: Map[Variable, Option[Variable]], parameters: Map[Definition, Option[Exp]]) {
  def ++(other: HeadMatch): HeadMatch =
    HeadMatch(generics ++ other.generics, dummies ++ other.dummies, parameters ++ other.parameters)

  def --(other: HeadMatch): HeadMatch =
    HeadMatch(generics -- other.generics.keys, dummies -- other.dummies.keys, parameters -- other.parameters.keys)

  def ||(other: HeadMatch): HeadMatch =
    HeadMatch(
      generics ++ (other.generics -- generics.filter(_._2.isDefined).keys),
      dummies ++ (other.dummies -- dummies.filter(_._2.isDefined).keys),
      parameters ++ (other.parameters -- parameters.filter(_._2.isDefined).keys),
    )

  def toSpecifier(header: Header): Specifier =
    Specifier(Some(header.generics.flatMap(generics.get)), Some(header.dummies.flatMap(dummies.get)), Some(header.parameters.flatMap(parameters.get)))
}
