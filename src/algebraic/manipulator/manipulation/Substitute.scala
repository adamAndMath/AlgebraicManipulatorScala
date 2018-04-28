package algebraic.manipulator.manipulation

import algebraic.manipulator._
import algebraic.manipulator.specifiers.Specifier

case class Substitute(positions: Tree, path: List[String], from: Int, to: Int, specifiers: List[Specifier]) extends PathManipulation(positions) {
  override def dependencies: Set[String] = Set(path.head)

  override def replace(env: Environment, exp: Exp): Exp =
    env.find(path, _.isInstanceOf[Substitutable])
      .getOrElse(throw new NoSuchElementException(path.mkString(".")))
      .asInstanceOf[Substitutable]
      .substitute(env, exp, specifiers, from, to)
}
