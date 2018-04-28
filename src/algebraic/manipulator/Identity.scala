package algebraic.manipulator

import algebraic.manipulator.manipulation.{Manipulation, Substitutable}
import algebraic.manipulator.specifiers.{Header, Specifier}

abstract class Identity(val header: Header, val result: List[Exp]) extends Substitutable {
  assert(result.flatMap(_.getBound).forall(header.dummies.contains))

  override def substitute(env: Environment, exp: Exp, specifiers: List[Specifier], from: Int, to: Int): Exp = {
    val headMatch = specifiers match {
      case Nil => header.toMatch
      case List(s) => s.headMatch(header)
      case _ => throw new IllegalArgumentException(s"An identity takes up to 1 specifier, but received ${specifiers.length}")
    }

    val fromExp = result(from)

    try {
      val re = fromExp.matchExp(exp, headMatch)
        .getOrElse(throw new IllegalStateException(s"Expected substitute of $fromExp, but received $exp"))

      val dums = re.dummies.map{case (p, o) => p -> o.getOrElse(throw new IllegalStateException(s"Undefined dummy $p in $exp"))}
      val pars = re.parameters.map{case (p, o) => p.variable -> o.getOrElse(throw new IllegalStateException(s"Undefined parameter $p in $exp"))}

      val fromSet = fromExp.setAll(dums, pars)

      if (fromSet != exp)
        throw new IllegalStateException(s"Expected $fromSet, but received $exp")

      result(to).setAll(dums, pars)
    } catch {
      case e: Exception => throw new IllegalArgumentException(s"Expected substitute of $fromExp, but received $exp", e)
    }
  }
}

class Assumption(header: Header, result: List[Exp]) extends Identity(header, result) {
  override def validate(name: String, env: Environment): Traversable[(List[String], String)] = None
}

class Proof(header: Header, result: List[Exp], val count: Int, val origin: Exp) extends Identity(header, result) {
  private var cur: List[Exp] = List.fill(count)(origin)
  private var manips: List[Manipulation] = List.empty

  def manipulations: List[Manipulation] = manips.reverse
  def current: List[Exp] = cur

  override def validate(name: String, env: Environment): Traversable[(List[String], String)] =
    Some(Nil -> s"The current state ${cur.mkString("=")} isn't equal to the expected result ${result.mkString("=")}")
      .filter(_ => (cur zip result).exists(p => p._1 != p._2))

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

  override def validate(name: String, env: Environment): Traversable[(List[String], String)] =
    Some(Nil -> s"The current state ${cur.mkString("=")} isn't equal to the expected result ${result.mkString("=")}")
      .filter(_ => (cur zip result).exists(p => p._1 != p._2))

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

  override def validate(name: String, env: Environment): Traversable[(List[String], String)] =
    base.validate(name, env).map{case (p,v) => ("base"::p) -> v} ++
      steps.flatMap{case (k, step) => step.flatMap(s => s.proof.validate(name, env).map{case (p,v) => (s"$k -> ${s.exp}"::p) -> v})}
}

sealed abstract class InductiveStep(header: Header, result: List[Exp], val inductives: Map[Variable, Exp], v: Variable, val params: List[Definition], val exp: Exp) {
  val proof: Identity

  def bindStep(env: Environment): Environment =
    env + ("step" -> new Assumption(header.mapPars(_.filterNot(d => inductives.contains(d.variable))), result))
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
}
