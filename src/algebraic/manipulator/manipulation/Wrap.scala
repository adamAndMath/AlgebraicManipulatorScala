package algebraic.manipulator.manipulation
import algebraic.manipulator._
import algebraic.manipulator.specifiers.HeadMatch

case class Wrap(wrapper: Exp, positions: Tree) extends PathManipulation(positions) {
  override def dependencies: Set[String] = wrapper.dependencies

  override def replace(env: Environment, exp: Exp): Exp = wrapper match {
    case Operation(Variable(name), _) => env.find(List(name), _.isInstanceOf[Wrapable]).get.asInstanceOf[Wrapable].wrap(wrapper, exp)
    case Variable(name) => env.find(List(name), _.isInstanceOf[Wrapable]).get.asInstanceOf[Wrapable].wrap(wrapper, exp)
    case Lambda(params, value) =>
      try {
        val re = value.matchExp(exp, HeadMatch(Map.empty, Map.empty, params.map(v => Definition(AnyType, v) -> None).toMap))
          .getOrElse(throw new IllegalStateException(s"Expected substitute of $value, but received $exp"))
        val pars = re.parameters.map { case (p, o) => p.variable -> o.getOrElse(throw new IllegalStateException(s"Undefined parameter $p in $exp")) }

        Operation(Lambda(params, value), params.map(pars))
      } catch {
        case e: Exception => throw new IllegalArgumentException(s"Expected substitute of $value, but received $exp", e)
      }
  }
}
