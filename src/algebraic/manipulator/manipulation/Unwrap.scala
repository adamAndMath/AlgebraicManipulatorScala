package algebraic.manipulator.manipulation

import algebraic.manipulator._
import algebraic.manipulator.function.{InductiveFunction, SimpleFunction}
import algebraic.manipulator.objects.SimpleObject

case class Unwrap(positions: Tree) extends PathManipulation(positions) {
  override def dependencies(env: Environment): Set[Path] = Set.empty

  override def replace(env: Environment, exp: Exp): Exp = exp match {
    case Variable(name) => env(Path(name)) match {
      case SimpleObject(value) => value
    }
    case Operation(name, dummies, parameters) => env(Path(name)) match {
      case SimpleFunction(header, value) =>
        if (header.dummies.length != dummies.length)
          throw new IllegalArgumentException(s"Expected ${header.dummies.length} dummies, but recieved ${dummies.length}")
        if (header.parameters.length != parameters.length)
          throw new IllegalArgumentException(s"Expected ${header.parameters.length} arguments, but recieved ${parameters.length}")

        val dumMap = header.dummies.zip(dummies).toMap
        val parMap = header.parameters.map(_.variable).zip(parameters).toMap
        value.setAll(dumMap, v => parMap.getOrElse(v, v))
      case InductiveFunction(header, base, steps) =>
        if (header.dummies.length != dummies.length)
          throw new IllegalArgumentException(s"Expected ${header.dummies.length} dummies, but recieved ${dummies.length}")
        if (header.parameters.length != parameters.length)
          throw new IllegalArgumentException(s"Expected ${header.parameters.length} arguments, but recieved ${parameters.length}")

        val dumMap = header.dummies.zip(dummies).toMap
        val parMap = header.parameters.map(_.variable).zip(parameters).toMap
        val inductive = parMap(base.inductive)

        if (base.value == inductive)
          return base.exp.setAll(dumMap, v => parMap.getOrElse(v, v))
        for (step <- steps) {
          val inner = inductive.get(step.step.tree.filter((base.inductive :: step.params.map(_.variable)).contains)).toMap
          if ((base.inductive :: step.params.map(_.variable)).forall(inner.contains)) {
            val indPar = step.step.set(v => inner.getOrElse(v, v))
            if (indPar == inductive) {
              val newParMap = parMap + (base.inductive -> indPar)
              return step.exp.setAll(dumMap, v => newParMap.getOrElse(v, v))
            }
          }
        }

        throw new IllegalArgumentException(s"No case in $name matches $exp")
    }
  }
}
