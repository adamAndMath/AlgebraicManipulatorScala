package algebraic.manipulator.manipulation

import algebraic.manipulator._

case class Rename(positions: Tree, from: Variable, to: Variable) extends PathManipulation(positions) {
  override def replace(env: Environment, exp: Exp): Exp = exp match {
    case Lambda(parameters, value) =>
      if (!parameters.contains(from))
        throw new IllegalStateException(s"$from is not defined at $exp")

      Lambda(parameters.map(v => if (v == from) to else v), value.set(from -> to))
  }

  override def dependencies(env: Environment): Set[Path] = Set.empty
}
