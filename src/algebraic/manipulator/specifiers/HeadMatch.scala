package algebraic.manipulator.specifiers

import algebraic.manipulator.{Definition, Exp, Variable}

case class HeadMatch(generics: Map[Variable, Option[Variable]], dummies: Map[Variable, Option[Variable]], parameters: Map[Definition, Option[Exp]]) {
  def ++(other: HeadMatch): HeadMatch =
    HeadMatch(generics ++ other.generics, dummies ++ other.dummies, parameters ++ other.parameters)

  def --(other: HeadMatch): HeadMatch =
    HeadMatch(generics -- other.generics.keys, dummies -- other.dummies.keys, parameters -- other.parameters.keys)
}
