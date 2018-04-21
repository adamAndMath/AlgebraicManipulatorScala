package algebraic.manipulator.manipulation

import algebraic.manipulator._

case class Substitute(positions: Tree, path: List[String], from: Int, to: Int, dummies: Option[List[Option[Variable]]], parameters: Option[List[Option[Exp]]]) extends PathManipulation(positions) {
  override def dependencies: Set[String] = Set(path.head)

  override def replace(env: Environment, exp: Exp): Exp = {
    val identity = env.find(path).asInstanceOf[Identity]
    val params: List[Option[Exp]] = parameters.getOrElse(List.fill(identity.header.parameters.length)(None))
    val bounds: List[Option[Variable]] = dummies.getOrElse(List.fill(identity.header.dummies.length)(None))

    if (bounds.length != identity.header.dummies.length)
      throw new IllegalStateException("Dummy count does't match referred work")

    if (params.length != identity.header.parameters.length)
      throw new IllegalStateException("Parameter count does't match referred work")

    val fromExp = identity.result(from)

    try {
      val re = fromExp.matchExp(exp, (identity.header.dummies zip bounds).toMap, (identity.header.parameters.map(_.variable) zip params).toMap)
        .getOrElse(throw new IllegalStateException(s"Expected substitute of $fromExp, but received $exp"))

      val dums = re._1.map{case (p, o) => p -> o.getOrElse(throw new IllegalStateException(s"Undefined dummy $p in $exp"))}
      val pars = re._2.map{case (p, o) => p -> o.getOrElse(throw new IllegalStateException(s"Undefined parameter $p in $exp"))}

      val fromSet = fromExp.setAll(dums, pars)

      if (fromSet != exp)
        throw new IllegalStateException(s"Expected $fromSet, but received $exp")

      identity.result(to).setAll(dums, pars)
    } catch {
      case e: Exception => throw new IllegalArgumentException(s"Expected substitute of $fromExp, but received $exp", e)
    }
  }
}
