package algebraic.manipulator.manipulation

import algebraic.manipulator._

case class FromEval(positions: Tree) extends PathManipulation(positions) {
  override def replace(finder: Project.Finder, exp: Exp): Exp = exp match {
    case Operation("eval", List(), Operation("func", vs, List(e)) :: ps) =>
      val map = (vs zip ps).toMap
      e.set(v => map.getOrElse(v, v))
  }
}
