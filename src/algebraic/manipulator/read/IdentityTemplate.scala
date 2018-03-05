package algebraic.manipulator.read

import algebraic.manipulator.Project.Finder
import algebraic.manipulator._
import algebraic.manipulator.manipulation.Manipulation

trait IdentityTemplate {
  def apply(finder: Project.Finder): Identity

  def dependencies(finder: Project.Finder): Set[List[String]]
}

case class AssumptionTemplate(header: Header, result: List[Exp]) extends IdentityTemplate {
  override def apply(finder: Finder): Assumption = new Assumption(header, result)

  override def dependencies(finder: Finder): Set[List[String]] = Set.empty
}

case class ProofTemplate(header: Header, result: List[Exp], count: Int, origin: Exp, manipulations: List[Manipulation]) extends IdentityTemplate {
  override def apply(finder: Finder): Proof = {
    val proof = new Proof(header, result, count, origin)
    (manipulations.indices zip manipulations).foreach{case (i, m) => try {proof(finder, m)} catch { case e: Exception => throw new IllegalStateException(s"Failed to apply manipulation $i: $m for ${proof.current.mkString("=")}", e)}}
    proof
  }

  override def dependencies(finder: Finder): Set[List[String]] = manipulations.map(_.dependencies(finder)).fold(Set.empty)(_ ++ _)
}

case class InductionProofTemplate(header: Header, result: List[Exp], inductives: Map[Variable, Exp], count: Int, origin: Exp, manipulations: List[Manipulation], up: Map[Variable, List[Manipulation]], down: Map[Variable, List[Manipulation]]) extends IdentityTemplate {
  override def apply(finder: Finder): InductionProof = {
    val proof = new InductionProof(header, result, inductives, count, origin)
    manipulations.foreach(proof.base(finder, _))
    up.foreach{ case (k, m) => m.foreach(proof.up(k)(finder, _))}
    proof
  }

  override def dependencies(finder: Finder): Set[List[String]] = (manipulations ++ (up.values ++ down.values).fold(List.empty)(_ ++ _)).map(_.dependencies(finder)).fold(Set.empty)(_ ++ _)
}
