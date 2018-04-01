package algebraic.manipulator.manipulation

import algebraic.manipulator._

case class Substitute(positions: Tree, path: Path, from: Int, to: Int, dummies: List[Variable], parameters: Option[List[Option[Exp]]]) extends PathManipulation(positions) {
  override def dependencies(env: Environment): Set[Path] = Set(env.toFull(path))

  override def replace(env: Environment, exp: Exp): Exp = {
    val identity = env(path).asInstanceOf[Identity]
    val params: List[Option[Exp]] = parameters.getOrElse(List.fill[Option[Exp]](identity.header.parameters.length)(None))

    if (this.dummies.length != identity.header.dummies.length)
      throw new IllegalStateException("Dummy count does't match referred work")

    if (params.length != identity.header.parameters.length)
      throw new IllegalStateException("Parameter count does't match referred work")

    val fromExp = identity.result(from)

    try {
      val pars = fromExp.matchExp(exp, (identity.header.parameters.map(_.variable) zip params).toMap)
        .getOrElse(throw new IllegalStateException(s"Expected substitute of $fromExp, but received $exp"))
        .map{ case (p, o) => p -> o.getOrElse(throw new IllegalStateException(s"Undefined parameter $p in $exp"))}

      val fromSet = fromExp.setAll((identity.header.dummies zip dummies).toMap, pars)

      if (fromSet != exp)
        throw new IllegalStateException(s"Expected $fromSet, but received $exp")

      identity.result(to).setAll((identity.header.dummies zip dummies).toMap, pars)
    } catch {
      case e: Exception => throw new IllegalArgumentException(s"Expected substitute of $fromExp, but received $exp", e)
    }
  }
}
