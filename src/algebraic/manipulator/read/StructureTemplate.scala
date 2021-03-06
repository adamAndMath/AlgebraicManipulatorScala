package algebraic.manipulator.read

import algebraic.manipulator.Environment
import algebraic.manipulator.read.ProofReader._
import algebraic.manipulator.read.Tokens._
import algebraic.manipulator.specifiers.Header
import algebraic.manipulator.structure._

trait StructureTemplate extends ElementTemplate {
  override def apply(name: String, env: Environment): Structure
}

object StructureTemplate {
  var readers: Map[String, Tokens => Read[StructureTemplate]] = Map.empty +
    ("assume" -> (readAssumption(_: Tokens))) +
    ("inductive" -> (readInductive(_: Tokens)))

  def readAssumption(tokens: Tokens.Tokens): Read[StructureTemplate] = (SimpleStructureTemplate, tokens)

  def readInductive(tokens: Tokens.Tokens): Read[InductiveStructureTemplate] = {
    val (header, t1) = readHeader(tokens)

    t1.expect(BLOCK, t1 => {
      val (base, t2) = readInductiveBase(t1)
      val (steps, t3) = t2.whileNot(CLOSE_BLOCK, readInductiveStep)
      (InductiveStructureTemplate(header, base, steps), t3)
    })
  }

  def readInductiveBase(tokens: Tokens): Read[InductiveBase] = {
    val (params, t1) = tokens.expect("base").whenBlock(PARENTHESES, _.readList(COMMA, readDefinition))
    val (exp, t2) = readExp(t1)
    (InductiveBase(params.getOrElse(Nil), exp), t2.ignore(SEMI))
  }

  def readInductiveStep(tokens: Tokens): Read[InductiveStep] = {
    val (v, t1) = readVariable(tokens.expect("step"))
    val (params, t2) = t1.whenBlock(PARENTHESES, _.readList(COMMA, readDefinition))
    val (exp, t3) = readExp(t2.expect(ARROW))
    (InductiveStep(v, params.getOrElse(Nil), exp), t3.ignore(SEMI))
  }

  object SimpleStructureTemplate extends StructureTemplate {
    override def apply(name: String, env: Environment): Structure = SimpleStructure

    override def dependencies: Set[String] = Set.empty
  }

  case class InductiveStructureTemplate(header: Header, base: InductiveBase, steps: List[InductiveStep]) extends StructureTemplate {
    override def apply(name: String, env: Environment): Structure = InductiveStructure(header, base, steps)

    override def dependencies: Set[String] = (base :: steps).flatMap(_.dependencies).toSet
  }
}