package algebraic.manipulator.manipulation
import algebraic.manipulator._
import algebraic.manipulator.function.{InductiveFunction, SimpleFunction}
import algebraic.manipulator.objects.SimpleObject

case class Wrap(wrapper: Exp, positions: Tree) extends PathManipulation(positions) {
  override def dependencies(env: Environment): Set[Path] = env.dependencies(wrapper)

  override def replace(env: Environment, exp: Exp): Exp = wrapper match {
    case Operation(name, dummies, parameters) => env(Path(name)) match {
      case SimpleFunction(header, value) =>
        if (header.dummies.length != dummies.length)
          throw new IllegalArgumentException(s"Expected ${header.dummies.length} dummies, but recieved ${dummies.length}")
        if (header.parameters.length != parameters.length)
          throw new IllegalArgumentException(s"Expected ${header.parameters.length} arguments, but recieved ${parameters.length}")
        val dumMap = header.dummies.zip(dummies).toMap
        val parMap = header.parameters.map(_.variable).zip(parameters).toMap
        val expect = value.setAll(dumMap, v => parMap.getOrElse(v, v))
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
    }
  }
}
