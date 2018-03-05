package algebraic.manipulator

import algebraic.manipulator.manipulation.Manipulation

case class Header(dummies: List[Variable], parameters: List[Definition]) {
  override def toString: String =
    if (dummies.isEmpty) s"(${parameters.mkString(",")})"
    else s"<${dummies.mkString(",")}(${parameters.mkString(",")})>"
}

abstract class Identity(val header: Header, val result: List[Exp]) {
  assert(result.flatMap(_.getFree).map(_.name).forall(header.parameters.map(_.name).contains))
  assert(result.flatMap(_.getBound).forall(header.dummies.contains))

  def validate(): Boolean
  def dependencies(finder: Project.Finder): Set[List[String]]
}

class Assumption(header: Header, result: List[Exp]) extends Identity(header, result) {
  override def validate(): Boolean = true

  override def dependencies(finder: Project.Finder): Set[List[String]] = Set.empty
}

class Proof(header: Header, result: List[Exp], val count: Int, val origin: Exp) extends Identity(header, result) {
  private var cur: List[Exp] = List.fill(count)(origin)
  private var manips: List[Manipulation] = List.empty

  def manipulations: List[Manipulation] = manips.reverse
  def current: List[Exp] = cur

  override def validate(): Boolean = (cur zip result).forall(p => p._1 == p._2)

  override def dependencies(finder: Project.Finder): Set[List[String]] = manips.map(_.dependencies(finder)).fold(Set.empty)(_ ++ _)

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
  private var current: List[Exp] = origin
  private var manips: List[Manipulation] = List.empty

  def manipulations: List[Manipulation] = manips.reverse

  override def validate(): Boolean = (current zip result).forall(p => p._1 == p._2)

  override def dependencies(finder: Project.Finder): Set[List[String]] = manips.map(_.dependencies(finder)).fold(Set.empty)(_ ++ _)

  def apply(finder: Project.Finder, manipulation: Manipulation): Unit = {
    current = manipulation(finder, current)
    manips ::= manipulation
  }

  def remove(finder: Project.Finder): Unit = {
    manips = manips.tail
    current = (manips :\ origin)(_(finder, _))
  }
}

class InductionProof(header: Header, result: List[Exp], val inductives: Map[Variable, Exp], val count: Int, val origin: Exp) extends Identity(header, result) {
  val base: Proof = new Proof(header, result.map(e => e.set(v => inductives.getOrElse(v, v))), count, origin)
  val up: Map[Variable, AssumedProof] = inductives.map{case (v, _) => (v, new AssumedProof(header, result.map(_.set(u => if (u == v) Operation("add", List.empty, List(v, IntVal(1))) else v)), result))}
  val down: Map[Variable, AssumedProof] = inductives.map{case (v, _) => (v, new AssumedProof(header, result.map(_.set(u => if (u == v) Operation("sub", List.empty, List(v, IntVal(1))) else v)), result))}

  override def validate(): Boolean = base.validate() && up.values.forall(_.validate()) && down.values.forall(_.validate())

  override def dependencies(finder: Project.Finder): Set[List[String]] = (List(base) ++ up.values ++ down.values).map(_.dependencies(finder)).fold(Set.empty)(_ ++ _)
}