package algebraic.manipulator.specifiers

import algebraic.manipulator.{Depending, Exp, Variable}

case class Specifier(generics: Option[List[Option[Variable]]], dummies: Option[List[Option[Variable]]], parameters: Option[List[Option[Exp]]]) extends Depending {
  override def dependencies: Set[String] =
    generics.toSet.flatten.flatten.map(_.name) ++
      parameters.toSet.flatten.flatten.flatMap(_.dependencies)

  def headMatch(header: Header): HeadMatch = {
    val gens = generics.getOrElse(List.fill(header.generics.length)(None))
    val dums = dummies.getOrElse(List.fill(header.dummies.length)(None))
    val pars = parameters.getOrElse(List.fill(header.parameters.length)(None))

    if (gens.length != header.generics.length)
      throw new IllegalStateException("Generic count does't match referred work")

    if (dums.length != header.dummies.length)
      throw new IllegalStateException("Dummy count does't match referred work")

    if (pars.length != header.parameters.length)
      throw new IllegalStateException("Parameter count does't match referred work")

    HeadMatch(header.generics.zip(gens).toMap, header.dummies.zip(dums).toMap, header.parameters.zip(pars).toMap)
  }

  def equivalent(other: Specifier): Boolean = {
    def comp[T](l1: Option[List[Option[T]]], l2: Option[List[Option[T]]]): Boolean =
      l1.flatMap(a => l2.map(b => a.length == b.length && (a zip b).forall{case (a1,b1) => a1.flatMap(a2 => b1.map(b2 => a2==b2)).getOrElse(true)})).getOrElse(true)

    comp(generics, other.generics) &&
    comp(dummies, other.dummies) &&
    comp(parameters, other.parameters)
  }
}
