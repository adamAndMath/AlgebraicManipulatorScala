package algebraic.manipulator.read

import algebraic.manipulator.properties.{PropImpl, Property}
import algebraic.manipulator._
import algebraic.manipulator.manipulation.Manipulation
import algebraic.manipulator.read.IdentityTemplate._
import algebraic.manipulator.read.ProofReader._
import algebraic.manipulator.read.Tokens.{BLOCK, CLOSE_BLOCK, EQUAL, IMPLY, Read, SEMI, Tokens}
import algebraic.manipulator.specifiers.{Header, Specifier}

object PropertyTemplate {
  var readers: Map[String, Tokens => Read[ElementTemplate]] = Map.empty +
    ("define" -> (readDefinition(_: Tokens))) +
    ("assume" -> (readAssumption(_: Tokens))) +
    ("prove" -> (readProof(_: Tokens))) +
    ("inductive" -> (readInduction(_: Tokens)))

  def readDefinition(tokens: Tokens.Tokens): Read[PropTemplate] = {
    val (header, t1) = readHeader(tokens)
    val (headerIde, t2) = readHeader(t1.expect(IMPLY))
    val (ide, t3) = t2.readList(EQUAL, readExp)

    (PropTemplate(header, headerIde, ide), t3)
  }

  def readAssumption(tokens: Tokens.Tokens): Read[PropAssumptionTemplate] = {
    val (spec, tail) = readSpecifier(tokens)
    (PropAssumptionTemplate(spec), tail)
  }

  def readProof(tokens: Tokens.Tokens): Read[PropImplTemplate] = {
    val (spec, t1) = readSpecifier(tokens)
    val (head, t2) = readHeader(t1)
    val ((count, origin, ms), t3) = t2.expect(BLOCK, tokens => {
      val (count, t1) = tokens.expect("let").int()
      val (origin, t2) = readExp(t1)
      val (ms, t3) = t2.ignore(SEMI).whileNot(CLOSE_BLOCK, readManipulation)
      ((count, origin, ms), t3)
    })
    (PropProofTemplate(spec, head, count, origin, ms), t3)
  }

  def readInduction(tokens: Tokens.Tokens): Read[PropImplTemplate] = {
    val (spec, t1) = readSpecifier(tokens)
    val (head, t2) = readHeader(t1)
    val ((base, steps), t3) = t2.expect(BLOCK, tokens => {
      val (base, t1) = readInductionBase(tokens)
      val (steps, t2) = t1.whileNot(CLOSE_BLOCK, readInductiveStep)
      ((base, steps), t2)
    })
    (PropInductiveTemplate(spec, head, base, steps), t3)
  }

  case class PropTemplate(header: Header, headerIde: Header, ide: List[Exp]) extends ElementTemplate {
    override def apply(name: String, env: Environment): Element = Property(name, header, headerIde, ide)

    override def dependencies: Set[String] = header.scope(headerIde.scope(ide.flatMap(_.dependencies).toSet))
  }

  trait PropImplTemplate extends ElementTemplate {
    val specifier: Specifier
    def proof(property: Property, env: Environment, result: List[Exp]): Identity

    override def apply(name: String, env: Environment): PropImpl = {
      val property = env.find(List(name), _.isInstanceOf[Property]).get.asInstanceOf[Property]
      val headMatch = specifier.headMatch(property.header)
      val result = property.ide.map(_.setAll(headMatch.dummies.mapValues(_.get), headMatch.parameters.map(p => p._1.variable -> p._2.get)))
      PropImpl(property, specifier, proof(property, env, result))
    }
  }

  case class PropAssumptionTemplate(override val specifier: Specifier) extends PropImplTemplate {
    override def proof(property: Property, env: Environment, result: List[Exp]): Assumption =
      new Assumption(property.ideHeader, result)

    override def dependencies: Set[String] = specifier.dependencies
  }

  case class PropProofTemplate(override val specifier: Specifier, header: Header, count: Int, origin: Exp, manipulations: List[Manipulation]) extends PropImplTemplate {
    override def proof(property: Property, env: Environment, result: List[Exp]): Proof = {
      val dum = (property.ideHeader.dummies zip header.dummies).toMap
      val par = (property.ideHeader.parameters.map(_.variable) zip header.parameters.map(_.variable)).toMap
      ProofTemplate(header, result.map(_.setAll(dum, par)), count, origin, manipulations)(property.name, env)
    }

    override def dependencies: Set[String] =
      specifier.dependencies ++ header.scopeWithDummies(manipulations.flatMap(_.dependencies).toSet)
  }

  case class PropInductiveTemplate(override val specifier: Specifier, header: Header, base: InductiveBaseTemplate, steps: List[InductiveStepTemplate]) extends PropImplTemplate {
    override def proof(property: Property, env: Environment, result: List[Exp]): Identity = {
      val dum = (property.ideHeader.dummies zip header.dummies).toMap
      val par = (property.ideHeader.parameters.map(_.variable) zip header.parameters.map(_.variable)).toMap
      InductionProofTemplate(header, result.map(_.setAll(dum, par)), base, steps)(property.name, env)
    }

    override def dependencies: Set[String] =
      specifier.dependencies ++ header.scopeWithDummies((base :: steps).flatMap(_.dependencies).toSet)
  }
}
