package algebraic.manipulator

import algebraic.manipulator.manipulation.Manipulation

case class Header(dummies: List[Variable], parameters: List[Definition]) extends Depending {
  override def dependencies(env: Environment): Set[Path] = env.dependencies(parameters)

  override def toString: String =
    if (dummies.isEmpty) s"(${parameters.mkString(",")})"
    else s"<${dummies.mkString(",")}(${parameters.mkString(",")})>"

  def bind(env: Environment): Environment = env.bind(parameters)
}

abstract class Identity(val header: Header, val result: List[Exp]) extends Element {
  assert(result.flatMap(_.getBound).forall(header.dummies.contains))

  def validate(): Boolean
}

class Assumption(header: Header, result: List[Exp]) extends Identity(header, result) {
  override def validate(): Boolean = true

  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(header) ++ header.bind(env).dependencies(result)
}

class Proof(header: Header, result: List[Exp], val count: Int, val origin: Exp) extends Identity(header, result) {
  private var cur: List[Exp] = List.fill(count)(origin)
  private var manips: List[Manipulation] = List.empty

  def manipulations: List[Manipulation] = manips.reverse
  def current: List[Exp] = cur

  override def validate(): Boolean = (cur zip result).forall(p => p._1 == p._2)

  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(header) ++ header.bind(env).dependencies(origin :: result ++ manips)

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

  override def validate(): Boolean = (cur zip result).forall(p => p._1 == p._2)

  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(header) ++ header.bind(env).dependencies(origin ++ result ++ manips)

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
  val base: Proof = new Proof(header, result.map(e => e.set(v => inductives.getOrElse(v, v))), count, origin)
  private var steps: Map[Variable, List[InductiveStep]] = inductives.map(_._1 -> Nil)

  def apply(v: Variable): List[InductiveStep] = steps(v)

  def addStep(v: Variable, params: List[Definition], exp: Exp): InductiveStep = {
    val step = new InductiveStep(header, result, v, params, exp)
    steps += (v -> (steps(v) :+ step))
    step
  }

  override def validate(): Boolean = base.validate() && steps.values.forall(_.forall(_.proof.validate()))

  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(header) ++ header.bind(env).dependencies(base :: origin :: result ++ inductives.values ++ steps.values.flatten)
}

class InductiveStep(header: Header, result: List[Exp], v: Variable, val params: List[Definition], val exp: Exp) extends Depending {
  val proof: AssumedProof = new AssumedProof(Header(header.dummies, header.parameters ++ params), result.map(_.set(u => if (u != v) u else exp)), result)

  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(header :: proof :: params) ++ env.bind(params).dependencies(exp)
}