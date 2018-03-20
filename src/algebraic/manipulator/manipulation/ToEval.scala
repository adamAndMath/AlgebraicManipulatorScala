package algebraic.manipulator.manipulation

import algebraic.manipulator._

object ToEval {
  case class Parameter(variable: Variable, exp: Option[Exp], positions: Option[Tree]) extends Depending {
    assume(exp.nonEmpty || positions.nonEmpty)

    override def dependencies(env: Environment): Set[Path] = exp.map(env.dependencies(_)).getOrElse(Set.empty)
  }
}

case class ToEval(positions: Tree, parameters: List[ToEval.Parameter]) extends PathManipulation(positions) {
  override def replace(env: Environment, exp: Exp): Exp = {
    val pars: Map[Variable, Exp] = parameters.map(p => p.variable -> {
      val es : List[Exp] = if (p.positions.nonEmpty) exp.get(p.positions.get).toList else List.empty
      val e = p.exp.getOrElse(es.head)

      es.foreach(e1 => if (e1 != e) throw new IllegalStateException(s"Expected $e but received $e1"))

      e
    }).toMap

    val valTree = (PathTree.empty[Variable] /: parameters.filter(_.positions.nonEmpty).map(p => p.positions.get :> p.variable))((a, b) => a|b)
    Operation("eval", List.empty, Operation("func", parameters.map(_.variable), List(exp.replace[Variable](valTree, (_, e) => e))) :: parameters.map(p => pars(p.variable)))
  }

  override def dependencies(env: Environment): Set[Path] = Set.empty
}