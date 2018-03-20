package algebraic.manipulator.read

import algebraic.manipulator._
import algebraic.manipulator.manipulation.Manipulation
import algebraic.manipulator.read.ProofReader._
import algebraic.manipulator.read.Tokens._

trait IdentityTemplate extends ElementTemplate {
  override def apply(env: Environment): Identity
}

object IdentityTemplate {
  var readers: Map[String, Tokens => Read[IdentityTemplate]] = Map.empty +
    ("assume" -> (readAssumption(_: Tokens))) +
    ("work" -> (readProof(_: Tokens))) + //TODO:Change work keyword to proof
    ("induction" -> (readInduction(_: Tokens)))

  def readAssumption(tokens: Tokens): Read[AssumptionTemplate] = {
    val (head, t1) = readHeader(tokens)
    val (res, t2) = t1.readList(EQUAL, readExp)
    (AssumptionTemplate(head, res), t2.ignore(SEMI))
  }

  def readProof(tokens: Tokens): Read[ProofTemplate] = {
    val (head, t1) = readHeader(tokens)
    val ((count, origin, ms), t2) = t1.expect(BLOCK, tokens => {
      val (count, t1) = tokens.expect("let").int()
      val (origin, t2) = readExp(t1)
      val (ms, t3) = t2.ignore(SEMI).whileNot(CLOSE_BLOCK, readManipulation)
      ((count, origin, ms), t3)
    })
    val (result, t3) = t2.expect(STRING("result")).ignore(BLOCK, tokens => {
      val (result, tail) = tokens.readList(EQUAL, readExp)
      (result, tail.ignore(SEMI))
    })
    (ProofTemplate(head, result, count, origin, ms), t3)
  }

  def readInductionBase(tokens: Tokens): Read[InductiveBaseTemplate] = {
    val (base, t1) = tokens.expect("base").readList(COMMA, tokens => {
      val (v, t1) = readVariable(tokens)
      val (e, t2) = readExp(t1.expect(EQUAL))
      ((v, e), t2)
    })
    val ((count:Int, origin:Exp, baseManip:List[Manipulation]), t2) = t1.expect(BLOCK, tokens => {
      val (count, t1) = tokens.expect("let").int()
      val (origin, t2) = readExp(t1)
      val (baseManip, t3) = t2.ignore(SEMI).whileNot(CLOSE_BLOCK, readManipulation)
      ((count, origin, baseManip), t3)
    })
    (InductiveBaseTemplate(base.toMap, count, origin, baseManip), t2)
  }

  def readInductiveStep(tokens: Tokens): Read[InductiveStepTemplate] = {
    val (v, t1) = readVariable(tokens)
    val (params, t2) = t1.whenBlock(PARENTHESES, _.readList(COMMA, readDefinition))
    val (exp, t3) = readExp(t2.expect(ARROW))
    val (ms, t4) = t3.expect(BLOCK, _.whileNot(CLOSE_BLOCK, readManipulation))
    (InductiveStepTemplate(v, params.getOrElse(Nil), exp, ms), t4)
  }

  def readInduction(tokens: Tokens): Read[InductionProofTemplate] = {
    val (head, t1) = readHeader(tokens)
    val ((base, steps), t2) = t1.expect(BLOCK, tokens => {
      val (base, t1) = readInductionBase(tokens)
      val (steps, t2) = t1.whileNot(CLOSE_BLOCK, readInductiveStep)
      ((base, steps), t2)
    })
    val (result, t3) = t2.expect(STRING("result")).ignore(BLOCK, tokens => {
      val (result, tail) = tokens.readList(EQUAL, readExp)
      (result, tail.ignore(SEMI))
    })
    (InductionProofTemplate(head, result, base, steps), t3)
  }

  case class AssumptionTemplate(header: Header, result: List[Exp]) extends IdentityTemplate {
    override def apply(env: Environment): Assumption = new Assumption(header, result)

    override def dependencies(env: Environment): Set[Path] =
      env.dependencies(header) ++ header.bind(env).dependencies(result)
  }

  case class ProofTemplate(header: Header, result: List[Exp], count: Int, origin: Exp, manipulations: List[Manipulation]) extends IdentityTemplate {
    override def apply(env: Environment): Proof = {
      val proof = new Proof(header, result, count, origin)
      (manipulations.indices zip manipulations).foreach { case (i, m) =>
        try {
          proof(env, m)
        } catch {
          case e: Exception => throw new IllegalStateException(s"Failed to apply manipulation ${i + 1}: $m for ${proof.current.mkString("=")}", e)
        }
      }
      proof
    }

    override def dependencies(env: Environment): Set[Path] =
      env.dependencies(header) ++ header.bind(env).dependencies(manipulations)
  }

  case class InductionProofTemplate(header: Header, result: List[Exp], base: InductiveBaseTemplate, steps: List[InductiveStepTemplate]) extends IdentityTemplate {
    override def apply(env: Environment): InductionProof = {
      val proof = new InductionProof(header, result, base.inductives, base.count, base.origin)
      (base.manipulations.indices zip base.manipulations).foreach { case (i, m) =>
        try {
          proof.base(env, m)
        } catch {
          case e: Exception => throw new IllegalStateException(s"Failed to apply manipulation ${i + 1} in base: $m for ${proof.base.current.mkString("=")}", e)
        }
      }
      steps.foreach(step => {
        val obj = proof.addStep(step.v, step.params, step.exp)
        (step.manipulations.indices zip step.manipulations).foreach { case (i, m) =>
          try {
            obj.proof(env, m)
          } catch {
            case e: Exception => throw new IllegalStateException(s"Failed to apply manipulation ${i + 1} in ${step.v} -> ${step.exp}: $m for ${obj.proof.current.mkString("=")}", e)
          }
        }
      })
      proof
    }

    override def dependencies(env: Environment): Set[Path] =
      env.dependencies(header) ++ header.bind(env).dependencies(base :: steps)
  }

  case class InductiveBaseTemplate(inductives: Map[Variable, Exp], count: Int, origin: Exp, manipulations: List[Manipulation]) extends Depending {
    override def dependencies(env: Environment): Set[Path] =
      env.dependencies(origin :: manipulations ++ inductives.values)
  }

  case class InductiveStepTemplate(v: Variable, params: List[Definition], exp: Exp, manipulations: List[Manipulation]) extends Depending {
    override def dependencies(env: Environment): Set[Path] =
      env.dependencies(params) ++ env.bind(params).dependencies(exp :: manipulations)
  }
}