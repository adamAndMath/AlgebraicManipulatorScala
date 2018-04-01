package algebraic.manipulator.read

import algebraic.manipulator.{Environment, Exp, Header, Path, Type}
import algebraic.manipulator.function._
import algebraic.manipulator.read.ProofReader._
import algebraic.manipulator.read.Tokens._

trait FunctionTemplate extends ElementTemplate {
  override def apply(env: Environment): FunctionElement
}

object FunctionTemplate {
  var readers: Map[String, Tokens => Read[FunctionTemplate]] = Map.empty +
    ("assume" -> (readAssumption(_: Tokens))) +
    ("define" -> (readDefine(_: Tokens))) +
    ("inductive" -> (readInductive(_: Tokens)))

  def readAssumption(tokens: Tokens.Tokens): Read[AssumedFunctionTemplate] = {
    val (params, tail) = tokens.expect(PARENTHESES, _.readList(COMMA, readType))

    (AssumedFunctionTemplate(params), tail.ignore(SEMI))
  }

  def readDefine(tokens: Tokens.Tokens): Read[SimpleFunctionTemplate] = {
    val (header, t1) = readHeader(tokens)
    val (exp, t2) = readExp(t1.expect(EQUAL))

    (SimpleFunctionTemplate(header, exp), t2.ignore(SEMI))
  }

  def readInductive(tokens: Tokens.Tokens): Read[InductiveFunctionTemplate] = {
    val (header, tail) = readHeader(tokens)
    tail.expect(BLOCK, tokens => {
      val (base, t1) = readInductiveBase(tokens)
      val (steps, t2) = t1.whileNot(CLOSE_BLOCK, readInductiveStep)

      (InductiveFunctionTemplate(header, base, steps), t2)
    })
  }

  def readInductiveBase(tokens: Tokens): Read[InductiveBase] = {
    val (inductive, t1) = readVariable(tokens.expect("base"))
    val (value, t2) = readExp(t1.expect(EQUAL))
    val (exp, t3) = readExp(t2.expect(IMPLY))

    (InductiveBase(inductive, value, exp), t3.ignore(SEMI))
  }

  def readInductiveStep(tokens: Tokens): Read[InductiveStep] = {
    val (params, t1) = tokens.expect("step").whenBlock(PARENTHESES, _.readList(COMMA, readDefinition))
    val (step, t2) = readExp(t1)
    val (exp, t3) = readExp(t2.expect(IMPLY))

    (InductiveStep(params.getOrElse(Nil), step, exp), t3.ignore(SEMI))
  }

  case class AssumedFunctionTemplate(params: List[Type]) extends FunctionTemplate {
    override def apply(env: Environment): FunctionElement = AssumedFunction(params)

    override def dependencies(env: Environment): Set[Path] = env.dependencies(params)
  }

  case class SimpleFunctionTemplate(header: Header, exp: Exp) extends FunctionTemplate {
    override def apply(env: Environment): FunctionElement = SimpleFunction(header, exp)

    override def dependencies(env: Environment): Set[Path] =
      env.dependencies(header) ++ header.bind(env).dependencies(exp)
  }

  case class InductiveFunctionTemplate(header: Header, base: InductiveBase, steps: List[InductiveStep]) extends FunctionTemplate {
    override def apply(env: Environment): FunctionElement = InductiveFunction(header, base, steps)

    override def dependencies(env: Environment): Set[Path] =
      env.dependencies(header) ++ header.bind(env).dependencies(base :: steps)
  }
}
