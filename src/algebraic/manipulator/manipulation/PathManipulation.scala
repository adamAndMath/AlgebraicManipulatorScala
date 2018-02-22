package algebraic.manipulator.manipulation

import algebraic.manipulator._

abstract class PathManipulation(positions: Tree) extends Manipulation {
  override def apply(finder: Project.Finder, equation: List[Exp]): List[Exp] = positions match {
    case n @ Tree.Node(c) =>
      if (n.min < 0 || n.max >= equation.size)
        throw new IllegalStateException("Illegal Path")

      (equation.indices zip equation).map { case (i, e) => if (c.contains(i)) e.replace(c(i), replace(finder, _)) else e }.toList
  }

  def replace(finder: Project.Finder, exp: Exp): Exp
}
