package algebraic.manipulator.manipulation

import algebraic.manipulator._
import algebraic.manipulator.function.{InductiveFunction, SimpleFunction}
import algebraic.manipulator.objects.SimpleObject

case class Unwrap(positions: Tree) extends PathManipulation(positions) {
  override def dependencies(env: Environment): Set[Path] = Set.empty

  override def replace(env: Environment, exp: Exp): Exp = exp match {
    case Variable(name) =>
      env(Path(name)) match {
        case SimpleObject(value) => value
      }
    case Operation(Variable(name), parameters) =>
      env(Path(name)) match {
        case SimpleFunction(header, value) =>
          if (header.parameters.length != parameters.length)
            throw new IllegalArgumentException(s"Expected ${header.parameters.length} arguments, but recieved ${parameters.length}")

          val parMap = header.parameters.map(_.variable).zip(parameters).toMap
          value.set(parMap)
        case InductiveFunction(header, base, steps) =>
          if (header.parameters.length != parameters.length)
            throw new IllegalArgumentException(s"Expected ${header.parameters.length} arguments, but recieved ${parameters.length}")

          val parMap = header.parameters.map(_.variable).zip(parameters).toMap
          val inductive = parMap(base.inductive)

          if (base.value == inductive)
            return base.exp.set(parMap)
          for (step <- steps) {
            val inner = inductive.get(step.step.tree.filter((base.inductive :: step.params.map(_.variable)).contains)).toMap
            if ((base.inductive :: step.params.map(_.variable)).forall(inner.contains)) {
              val indPar = step.step.set(inner)
              if (indPar == inductive) {
                val newParMap = parMap ++ inner
                return step.exp.set(newParMap)
              }
            }
          }

          throw new IllegalArgumentException(s"No case in $name matches $inductive")
      }
    case Operation(Lambda(params, value), args) =>
      if (params.length != args.length)
        throw new IllegalArgumentException(s"Expected ${params.length} arguments, but recieved ${args.length}")

      value.set((params zip args).toMap)
    case Lambda(params, Operation(op, args)) =>
      if (params != args)
        throw new IllegalArgumentException(s"Expected the arguments $params, but recieved $args")

      op
  }
}
