package algebraic.manipulator.manipulation
import algebraic.manipulator._
import algebraic.manipulator.function.{InductiveFunction, SimpleFunction}
import algebraic.manipulator.objects.SimpleObject

case class Wrap(wrapper: Exp, positions: Tree) extends PathManipulation(positions) {
  override def dependencies(env: Environment): Set[Path] = env.dependencies(wrapper)

  override def replace(env: Environment, exp: Exp): Exp = wrapper match {
    case Operation(Variable(name), parameters) => env(Path(name)) match {
      case SimpleFunction(header, value) =>
        if (header.parameters.length != parameters.length)
          throw new IllegalArgumentException(s"Expected ${header.parameters.length} arguments, but recieved ${parameters.length}")
        val parMap = header.parameters.map(_.variable).zip(parameters).toMap
        val expect = value.set(parMap)
        if (expect != exp)
          throw new IllegalArgumentException(s"Expected $expect, but recieved $exp")
        wrapper
      case InductiveFunction(header, base, steps) => ???
    }
    case Variable(name) => env(Path(name)) match {
      case SimpleObject(value) =>
        if (value != exp)
          throw new IllegalArgumentException(s"Expected $value, but recieved $exp")
        wrapper
      case SimpleFunction(Header(_, params), value) =>
        try {
          val pars = value.matchExp(exp, params.map(_.variable -> None).toMap)
            .getOrElse(throw new IllegalStateException(s"Expected substitute of $value, but received $exp"))
            .map{ case (p, o) => p -> o.getOrElse(throw new IllegalStateException(s"Undefined parameter $p in $exp"))}

          Operation(Variable(name), params.map(d => pars(d.variable)))
        } catch {
          case e: Exception => throw new IllegalArgumentException(s"Expected substitute of $value, but received $exp", e)
        }
    }
    case Lambda(params, value) =>
      try {
        val pars = value.matchExp(exp, params.map(_ -> None).toMap)
          .getOrElse(throw new IllegalStateException(s"Expected substitute of $value, but received $exp"))
          .map { case (p, o) => p -> o.getOrElse(throw new IllegalStateException(s"Undefined parameter $p in $exp")) }

        Operation(Lambda(params, value), params.map(pars))
      } catch {
        case e: Exception => throw new IllegalArgumentException(s"Expected substitute of $value, but received $exp", e)
      }
  }
}
