package algebraic.manipulator

import algebraic.manipulator.manipulation.{Manipulation, Substitutable}
import algebraic.manipulator.specifiers.{HeadMatch, Header, Specifier}

abstract class Identity(val header: Header, val result: List[Exp]) extends Element with Substitutable {
  assert(result.flatMap(_.getBound).forall(header.dummies.contains))

  override def substitute(specifiers: List[Specifier]): (List[Exp], HeadMatch) = specifiers match {
    case Nil => (result, header.toMatch)
    case List(s) => (result, s.headMatch(header))
    case _ => throw new IllegalArgumentException(s"An identity takes up to 1 specifier, but received ${specifiers.length}")
  }
}

class Assumption(header: Header, result: List[Exp]) extends Identity(header, result) {
  override def validate(env: Environment): Traversable[(List[String], String)] = None

  override def dependencies: Set[String] =
    header.scope(result.flatMap(_.dependencies).toSet)
}

class Proof(header: Header, result: List[Exp], val count: Int, val origin: Exp) extends Identity(header, result) {
  private var cur: List[Exp] = List.fill(count)(origin)
  private var manips: List[Manipulation] = List.empty

  def manipulations: List[Manipulation] = manips.reverse
  def current: List[Exp] = cur

  override def validate(env: Environment): Traversable[(List[String], String)] =
    Some(Nil -> s"The current state ${cur.mkString("=")} isn't equal to the expected result ${result.mkString("=")}")
      .filter(_ => (cur zip result).exists(p => p._1 != p._2))

  override def dependencies: Set[String] =
    header.scopeWithDummies((origin :: result ++ manips).flatMap(_.dependencies).toSet)

  def apply(env: Environment, manipulation: Manipulation): Unit = {
    cur = manipulation(env, cur)
    manips ::= manipulation
  }

  def remove(env: Environment): Unit = {
    manips = manips.tail
    cur = (manips :\ List.fill(count)(origin))(_(env, _))
  }
}

class AssumedProof(header: Header, result: List[Exp], val origin: List[Exp]) extends Identity(header, result) {
  private var cur: List[Exp] = origin
  private var manips: List[Manipulation] = List.empty

  def manipulations: List[Manipulation] = manips.reverse
  def current: List[Exp] = cur

  override def validate(env: Environment): Traversable[(List[String], String)] =
    Some(Nil -> s"The current state ${cur.mkString("=")} isn't equal to the expected result ${result.mkString("=")}")
      .filter(_ => (cur zip result).exists(p => p._1 != p._2))

  override def dependencies: Set[String] =
    header.scope((origin ++ result ++ manips).flatMap(_.dependencies).toSet)

  def apply(env: Environment, manipulation: Manipulation): Unit = {
    cur = manipulation(env, cur)
    manips ::= manipulation
  }

  def remove(env: Environment): Unit = {
    manips = manips.tail
    cur = (manips :\ origin)(_(env, _))
  }
}

class InductionProof(header: Header, result: List[Exp], val inductives: Map[Variable, Exp], count: Int, origin: Exp) extends Identity(header, result) {
  val base: Proof = new Proof(header, result.map(_.set(inductives)), count, origin)
  private var steps: Map[Variable, List[InductiveStep]] = inductives.map(_._1 -> Nil)

  def apply(v: Variable): List[InductiveStep] = steps(v)

  def addStep(v: Variable, params: List[Definition], exp: Exp): InductiveStep = {
    val step = new InductiveAssumedStep(header, result, inductives, v, params, exp)
    steps += (v -> (steps(v) :+ step))
    step
  }

  def addStep(v: Variable, params: List[Definition], exp: Exp, count: Int, origin: Exp): InductiveStep = {
    val step = new InductiveProofStep(header, result, inductives, v, params, exp, count, origin)
    steps += (v -> (steps(v) :+ step))
    step
  }

  override def validate(env: Environment): Traversable[(List[String], String)] =
    base.validate(env).map{case (p,v) => ("base"::p) -> v} ++
      steps.flatMap{case (k, step) => step.flatMap(s => s.proof.validate(env).map{case (p,v) => (s"$k -> ${s.exp}"::p) -> v})}

  override def dependencies: Set[String] =
    header.scopeWithDummies((base :: origin :: result ++ inductives.values ++ steps.values.flatten).flatMap(_.dependencies).toSet)
}

sealed abstract class InductiveStep(header: Header, result: List[Exp], val inductives: Map[Variable, Exp], v: Variable, val params: List[Definition], val exp: Exp) extends Depending {
  val proof: Identity

  def bindStep(env: Environment): Environment =
    env + ("step" -> new Assumption(header.mapPars(_.filterNot(d => inductives.contains(d.variable))), result))

  override def dependencies: Set[String] =
    Header(Nil, Nil, params).scope(proof.dependencies - "step" ++ exp.dependencies)
}

class InductiveAssumedStep(header: Header, result: List[Exp], inductives: Map[Variable, Exp], v: Variable, params: List[Definition], exp: Exp)
  extends InductiveStep(header: Header, result: List[Exp], inductives: Map[Variable, Exp], v: Variable, params: List[Definition], exp: Exp) {
  override val proof: AssumedProof =
    new AssumedProof(header.mapPars(_ ++ params), result.map(_.set(v -> exp)), result)
}

class InductiveProofStep(header: Header, result: List[Exp], inductives: Map[Variable, Exp], v: Variable, params: List[Definition], exp: Exp, count: Int, origin: Exp)
  extends InductiveStep(header: Header, result: List[Exp], inductives: Map[Variable, Exp], v: Variable, params: List[Definition], exp: Exp) {
  override val proof: Proof =
    new Proof(header.mapPars(_ ++ params), result.map(_.set(v -> exp)), count, origin)

  override def dependencies: Set[String] = super.dependencies ++ Header(Nil, Nil, params).scope(origin.dependencies)
}
