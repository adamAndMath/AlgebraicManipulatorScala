package algebraic.manipulator.manipulation

import algebraic.manipulator._

object ToEval {
  case class Parameter(variable: Variable, exp: Option[Exp], positions: Option[Tree]) {
    assume(exp.nonEmpty || positions.nonEmpty)
  }
}

case class ToEval(positions: Tree, parameters: List[ToEval.Parameter]) extends PathManipulation(positions) {
  override def replace(finder: Project.Finder, exp: Exp): Exp = {
    val pars: Map[Variable, Exp] = parameters.map(p => p.variable -> {
      val es : List[Exp] = if (p.positions.nonEmpty) exp.get(p.positions.get).toList else List.empty
      val e = p.exp.getOrElse(es.head)

      es.foreach(e1 => if (e1 != e) throw new IllegalStateException(s"Expected $e but received $e1"))

      e
    }).toMap

    val valTree: PathTree[Exp] = (PathTree.empty[Exp] /: parameters.filter(_.positions.nonEmpty).map(p => p.positions.get :> pars(p.variable)))((a, b) => a|b)
    Operation("eval", List.empty, Operation("func", parameters.map(_.variable), List(exp.replace[Exp](valTree, (_, e) => e))) :: parameters.map(p => pars(p.variable)))
  }
}