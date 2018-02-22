package algebraic.manipulator.manipulation

import algebraic.manipulator._

case class Rename(positions: Tree, from: Variable, to: Variable) extends PathManipulation(positions) {
  override def replace(finder: Project.Finder, exp: Exp): Exp = exp match {
    case Operation(name, dummies, parameters) =>
      if (!dummies.contains(from))
        throw new IllegalStateException(s"$from is not defined at $exp")

      Operation(name, dummies.map(v => if (v == from) to else v), parameters.map(_.set(v => if (v == from) to else v)))
  }
}
