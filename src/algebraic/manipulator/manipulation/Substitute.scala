package algebraic.manipulator.manipulation

import algebraic.manipulator._
import algebraic.manipulator.specifiers.Specifier

case class Substitute(positions: Tree, path: List[String], from: Int, to: Int, specifiers: List[Specifier]) extends PathManipulation(positions) {
  override def dependencies: Set[String] = Set(path.head)

  override def replace(env: Environment, exp: Exp): Exp = {
    val (identity, headMatch) = env.find(path, _.isInstanceOf[Substitutable]).get.asInstanceOf[Substitutable].substitute(specifiers)

    val fromExp = identity(from)

    try {
      val re = fromExp.matchExp(exp, headMatch)
        .getOrElse(throw new IllegalStateException(s"Expected substitute of $fromExp, but received $exp"))

      val dums = re.dummies.map{case (p, o) => p -> o.getOrElse(throw new IllegalStateException(s"Undefined dummy $p in $exp"))}
      val pars = re.parameters.map{case (p, o) => p.variable -> o.getOrElse(throw new IllegalStateException(s"Undefined parameter $p in $exp"))}

      val fromSet = fromExp.setAll(dums, pars)

      if (fromSet != exp)
        throw new IllegalStateException(s"Expected $fromSet, but received $exp")

      identity(to).setAll(dums, pars)
    } catch {
      case e: Exception => throw new IllegalArgumentException(s"Expected substitute of $fromExp, but received $exp", e)
    }
  }
}
