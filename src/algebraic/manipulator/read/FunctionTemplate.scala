package algebraic.manipulator.read

import algebraic.manipulator.{Environment, Exp}
import algebraic.manipulator.function._
import algebraic.manipulator.read.ProofReader._
import algebraic.manipulator.read.Tokens._
import algebraic.manipulator.specifiers.{Header, TypeHeader}

trait FunctionTemplate extends ElementTemplate {
  override def apply(env: Environment): FunctionElement
}

object FunctionTemplate {
  var readers: Map[String, Tokens => Read[FunctionTemplate]] = Map.empty +
    ("assume" -> (readAssumption(_: Tokens))) +
    ("define" -> (readDefine(_: Tokens))) +
    ("inductive" -> (readInductive(_: Tokens)))

  def readAssumption(tokens: Tokens.Tokens): Read[AssumedFunctionTemplate] = {
    val (header, tail) = readTypeHeader(tokens)

    (AssumedFunctionTemplate(header), tail.ignore(SEMI))
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

  case class AssumedFunctionTemplate(header: TypeHeader) extends FunctionTemplate {
    override def apply(env: Environment): FunctionElement = AssumedFunction(header)

    override def dependencies: Set[String] = header.dependencies
  }

  case class SimpleFunctionTemplate(header: Header, exp: Exp) extends FunctionTemplate {
    override def apply(env: Environment): FunctionElement = SimpleFunction(header, exp)

    override def dependencies: Set[String] =
      header.scope(exp.dependencies)
  }

  case class InductiveFunctionTemplate(header: Header, base: InductiveBase, steps: List[InductiveStep]) extends FunctionTemplate {
    override def apply(env: Environment): FunctionElement = InductiveFunction(header, base, steps)

    override def dependencies: Set[String] =
      header.scope((base :: steps).flatMap(_.dependencies).toSet)
  }
}
