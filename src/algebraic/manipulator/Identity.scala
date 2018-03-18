package algebraic.manipulator

import algebraic.manipulator.manipulation.Manipulation

case class Header(dummies: List[Variable], parameters: List[Definition]) extends Depending {
  override def dependencies(finder: Project.Finder): Set[Path] = parameters.map(_.dependencies(finder)).fold(Set.empty)(_++_)
  override def toString: String =
    if (dummies.isEmpty) s"(${parameters.mkString(",")})"
    else s"<${dummies.mkString(",")}(${parameters.mkString(",")})>"
}

abstract class Identity(val header: Header, val result: List[Exp]) extends Element {
  assert(result.flatMap(_.getFree).map(_.name).forall(header.parameters.map(_.name).contains))
  assert(result.flatMap(_.getBound).forall(header.dummies.contains))

  def validate(): Boolean
}

class Assumption(header: Header, result: List[Exp]) extends Identity(header, result) {
  override def validate(): Boolean = true

  override def dependencies(finder: Project.Finder): Set[Path] = header.dependencies(finder)
}

class Proof(header: Header, result: List[Exp], val count: Int, val origin: Exp) extends Identity(header, result) {
  private var cur: List[Exp] = List.fill(count)(origin)
  private var manips: List[Manipulation] = List.empty

  def manipulations: List[Manipulation] = manips.reverse
  def current: List[Exp] = cur

  override def validate(): Boolean = (cur zip result).forall(p => p._1 == p._2)

  override def dependencies(finder: Project.Finder): Set[Path] = (header :: manips).map(_.dependencies(finder)).fold(Set.empty)(_ ++ _)

  def apply(finder: Project.Finder, manipulation: Manipulation): Unit = {
    cur = manipulation(finder, cur)
    manips ::= manipulation
  }

  def remove(finder: Project.Finder): Unit = {
    manips = manips.tail
    cur = (manips :\ List.fill(count)(origin))(_(finder, _))
  }
}

class AssumedProof(header: Header, result: List[Exp], val origin: List[Exp]) extends Identity(header, result) {
  private var cur: List[Exp] = origin
  private var manips: List[Manipulation] = List.empty

  def manipulations: List[Manipulation] = manips.reverse
  def current: List[Exp] = cur

  override def validate(): Boolean = (cur zip result).forall(p => p._1 == p._2)

  override def dependencies(finder: Project.Finder): Set[Path] = (header :: manips).map(_.dependencies(finder)).fold(Set.empty)(_ ++ _)

  def apply(finder: Project.Finder, manipulation: Manipulation): Unit = {
    cur = manipulation(finder, cur)
    manips ::= manipulation
  }

  def remove(finder: Project.Finder): Unit = {
    manips = manips.tail
    cur = (manips :\ origin)(_(finder, _))
  }
}

class InductionProof(header: Header, result: List[Exp], val inductives: Map[Variable, Exp], count: Int, origin: Exp) extends Identity(header, result) {
  val base: Proof = new Proof(header, result.map(e => e.set(v => inductives.getOrElse(v, v))), count, origin)
  private var steps: Map[Variable, List[InductiveStep]] = inductives.map(_._1 -> Nil)

  def apply(v: Variable): List[InductiveStep] = steps(v)

  def addStep(v: Variable, params: List[Definition], exp: Exp): InductiveStep = {
    val step = new InductiveStep(header, result, v, params, exp)
    steps += (v -> (steps(v) :+ step))
    step
  }

  override def validate(): Boolean = base.validate() && steps.values.forall(_.forall(_.proof.validate()))

  override def dependencies(finder: Project.Finder): Set[Path] =
    (List(header, base) ++ steps.values.flatten).map(_.dependencies(finder)).fold(Set.empty)(_ ++ _)
}

class InductiveStep(header: Header, result: List[Exp], v: Variable, val params: List[Definition], val exp: Exp) extends Depending {
  val proof: AssumedProof = new AssumedProof(Header(header.dummies, header.parameters ++ params), result.map(_.set(u => if (u != v) u else exp)), result)

  override def dependencies(finder: Project.Finder): Set[Path] = List(header, proof).map(_.dependencies(finder)).fold(Set.empty)(_++_)
}