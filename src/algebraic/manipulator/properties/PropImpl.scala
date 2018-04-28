package algebraic.manipulator.properties

import algebraic.manipulator.specifiers.Specifier
import algebraic.manipulator.{Element, Environment, Identity}

case class PropImpl(property: Property, specifier: Specifier, identity: Identity) extends Element {
  override def validate(name: String, env: Environment): Traversable[(List[String], String)] =
    Some(Nil -> s"Illegal header ${identity.header}, expected ${property.ideHeader}")
      .filterNot(_ => property.ideHeader equivalent identity.header) ++
    Some(Nil -> s"Invalid result ${identity.result.mkString("=")}, expected subset of ${property.ide.mkString("=")}")
      .filterNot(_ => (Option(specifier.headMatch(property.header)++property.ideHeader.toMatch) /: property.ide.zip(identity.result))((m, e) => m.flatMap(e._1.matchExp(e._2, _))).isDefined) ++
    identity.validate(name, env)

  def matches(prop: Property, spec: Specifier): Boolean = prop == property && spec.equivalent(specifier)
}
