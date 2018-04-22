package algebraic.manipulator.specifiers

import algebraic.manipulator.{Exp, Variable}

case class Specifier(generics: Option[List[Option[Variable]]], dummies: Option[List[Option[Variable]]], parameters: Option[List[Option[Exp]]]) {
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
}
