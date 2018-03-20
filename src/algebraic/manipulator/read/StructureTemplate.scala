package algebraic.manipulator.read

import algebraic.manipulator.{Environment, Path}
import algebraic.manipulator.read.ProofReader._
import algebraic.manipulator.read.Tokens._
import algebraic.manipulator.structure._

trait StructureTemplate extends ElementTemplate {
  override def apply(env: Environment): Structure
}

object StructureTemplate {
  var readers: Map[String, Tokens => Read[StructureTemplate]] = Map.empty +
    ("assume" -> (readAssumption(_: Tokens))) +
    ("inductive" -> (readInductive(_: Tokens)))

  def readAssumption(tokens: Tokens.Tokens): Read[StructureTemplate] = (SimpleStructureTemplate, tokens)

  def readInductive(tokens: Tokens.Tokens): Read[InductiveStructureTemplate] = tokens.expect(BLOCK, tokens => {
    val (base, t1) = readInductiveBase(tokens)
    val (steps, t2) = t1.whileNot(CLOSE_BLOCK, readInductiveStep)
    (InductiveStructureTemplate(base, steps), t2)
  })

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
    override def apply(env: Environment): Structure = SimpleStructure

    override def dependencies(env: Environment): Set[Path] = Set.empty
  }

  case class InductiveStructureTemplate(base: InductiveBase, steps: List[InductiveStep]) extends StructureTemplate {
    override def apply(env: Environment): Structure = InductiveStructure(base, steps)

    override def dependencies(env: Environment): Set[Path] = (base :: steps).map(_.dependencies(env)).fold(Set.empty)(_++_)
  }
}