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
    val step = new InductiveAssumedStep(header, result, inductives, v, params, exp)
    steps += (v -> (steps(v) :+ step))
    step
  }

  def addStep(v: Variable, params: List[Definition], exp: Exp, count: Int, origin: Exp): InductiveStep = {
    val step = new InductiveProofStep(header, result, inductives, v, params, exp, count, origin)
    steps += (v -> (steps(v) :+ step))
    step
  }

  override def validate(): Boolean = base.validate() && steps.values.forall(_.forall(_.proof.validate()))

  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(header) ++ header.bind(env).dependencies(base :: origin :: result ++ inductives.values ++ steps.values.flatten)
}

sealed abstract class InductiveStep(header: Header, result: List[Exp], val inductives: Map[Variable, Exp], v: Variable, val params: List[Definition], val exp: Exp) extends Depending {
  val proof: Identity

  def bindStep(env: Environment): Environment = StepEnvironment(env, header, result, inductives)

  override def dependencies(env: Environment): Set[Path] =
    env.dependencies(params) ++ bindStep(env).dependencies(proof) ++ env.bind(params).dependencies(exp)
}

class InductiveAssumedStep(header: Header, result: List[Exp], inductives: Map[Variable, Exp], v: Variable, params: List[Definition], exp: Exp)
  extends InductiveStep(header: Header, result: List[Exp], inductives: Map[Variable, Exp], v: Variable, params: List[Definition], exp: Exp) {
  override val proof: AssumedProof =
    new AssumedProof(Header(header.dummies, header.parameters ++ params), result.map(_.set(u => if (u != v) u else exp)), result)
}

class InductiveProofStep(header: Header, result: List[Exp], inductives: Map[Variable, Exp], v: Variable, params: List[Definition], exp: Exp, count: Int, origin: Exp)
  extends InductiveStep(header: Header, result: List[Exp], inductives: Map[Variable, Exp], v: Variable, params: List[Definition], exp: Exp) {
  override val proof: Proof =
    new Proof(Header(header.dummies, header.parameters ++ params), result.map(_.set(u => if (u != v) u else exp)), count, origin)

  override def dependencies(env: Environment): Set[Path] = super.dependencies(env) ++ env.bind(params).dependencies(origin)
}

case class StepEnvironment(env: Environment, header: Header, result: List[Exp], inductives: Map[Variable, Exp]) extends Environment {
  val assumption = new Assumption(Header(header.dummies, header.parameters.filterNot(d => inductives.contains(d.variable))), result)
  override val path: Path = env.path + ""

  override def apply(path: Path): Element = if (path == Path("step")) assumption else env.apply(path)
  override def toFull(path: Path): Path = if (path == Path("step")) this.path + "step" else env.toFull(path)
}