package algebraic.manipulator.manipulation

import algebraic.manipulator._

case class FromEval(positions: Tree) extends PathManipulation(positions) {
  override def replace(env: Environment, exp: Exp): Exp = exp match {
    case Operation("eval", Nil, Operation("func", vs, List(e)) :: ps) =>
      if (vs.length != ps.length)
        throw new IllegalStateException
      val map = (vs zip ps).toMap
      e.set(v => map.getOrElse(v, v))
  }

  override def dependencies(env: Environment): Set[Path] = Set.empty
}
