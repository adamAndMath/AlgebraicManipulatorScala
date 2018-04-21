package algebraic.manipulator.read

import algebraic.manipulator.{Environment, Exp}
import algebraic.manipulator.objects._
import algebraic.manipulator.read.ProofReader.readExp
import algebraic.manipulator.read.Tokens._

trait ObjectTemplate extends ElementTemplate {
  override def apply(env: Environment): ObjectElement
}

object ObjectTemplate {
  var readers: Map[String, Tokens => Read[ObjectTemplate]] = Map.empty +
    ("assume" -> (readAssumption(_: Tokens))) +
    ("define" -> (readDefinition(_: Tokens)))

  def readAssumption(tokens: Tokens.Tokens): Read[ObjectTemplate] = (AssumedObjectTemplate, tokens.ignore(SEMI))

  def readDefinition(tokens: Tokens.Tokens): Read[SimpleObjectTemplate] = {
    val (exp, tail) = readExp(tokens.expect(EQUAL))
    (SimpleObjectTemplate(exp), tail.ignore(SEMI))
  }

  object AssumedObjectTemplate extends ObjectTemplate {
    override def apply(env: Environment): ObjectElement = AssumedObject

    override def dependencies: Set[String] = Set.empty
  }

  case class SimpleObjectTemplate(exp: Exp) extends ObjectTemplate {
    override def apply(env: Environment): ObjectElement = SimpleObject(exp)

    override def dependencies: Set[String] = exp.dependencies
  }
}
