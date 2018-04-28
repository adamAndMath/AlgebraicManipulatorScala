package algebraic.manipulator.properties

import algebraic.manipulator.manipulation.Substitutable
import algebraic.manipulator.specifiers.{Header, Specifier}
import algebraic.manipulator.{Environment, Exp}

case class Property(name: String, header: Header, ideHeader: Header, ide: List[Exp]) extends Substitutable {
  override def validate(name: String, env: Environment): Traversable[(List[String], String)] = None

  override def substitute(env: Environment, exp: Exp, specifiers: List[Specifier], from: Int, to: Int): Exp =
    impl(env, exp, specifiers, from).identity.substitute(env, exp, if (specifiers.isEmpty) Nil else specifiers.tail, from, to)

  def impl(env: Environment, exp: Exp, specifiers: List[Specifier], from: Int): PropImpl = {
    val headMatch = specifiers match {
      case List(s1, s2) => s1.headMatch(header) ++ s2.headMatch(ideHeader)
      case List(s) => s.headMatch(header) ++ ideHeader.toMatch
      case Nil => header.toMatch ++ ideHeader.toMatch
    }

    val fromExp = ide(from)
    val re = fromExp.matchExp(exp, headMatch)
      .getOrElse(throw new IllegalStateException(s"Expected substitute of $fromExp, but received $exp"))

    val implSpecifier = (re -- ideHeader.toMatch).toSpecifier(header)
    env.find(List(name), p => p.isInstanceOf[PropImpl] && p.asInstanceOf[PropImpl].matches(this, implSpecifier))
      .getOrElse(throw new NoSuchElementException(s"No implementation of the property $name matches the header $implSpecifier"))
      .asInstanceOf[PropImpl]
  }
}
