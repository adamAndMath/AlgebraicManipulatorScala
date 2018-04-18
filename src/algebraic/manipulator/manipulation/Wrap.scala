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
          throw new IllegalArgumentException(s"Expected $expect, but received $exp")
        wrapper
      case InductiveFunction(header, base, steps) =>
        val parMap = header.parameters.map(_.variable).zip(parameters).toMap
        val inductive = parMap(base.inductive)

        if (base.value == inductive) {
          val re = base.exp.matchExp(exp, base.exp.getBound.map(_ -> None).toMap, parMap.mapValues(Some(_)) - base.inductive)
            .getOrElse(throw new IllegalArgumentException(s"Expected ${base.exp.set(parMap)}, but received $exp"))
          val dums = re._1.map{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer dummy $p"))}
          val pars = re._2.map{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer parameter $p"))}
          wrapper
        } else {
          for (step <- steps) {
            val indRe = step.step.matchExp(inductive, step.step.getBound.map(_ -> None).toMap, step.params.map(_.variable -> None).toMap + (base.inductive -> None))
            if (indRe.exists(r => (r._1.values ++ r._2.values).forall(_.isDefined))) {
              val re = step.exp.matchExp(exp, step.exp.getBound.map(_ -> None).toMap, parMap.mapValues(Some(_)) ++ indRe.get._2)
                .getOrElse(throw new IllegalArgumentException(s"Expected substitute of ${step.exp.set(indRe.get._2.mapValues(_.get))}, but received $exp"))
              val dums = re._1.map{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer dummy $p"))}
              val pars = re._2.map{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer parameter $p"))}
              return wrapper
            }
          }

          throw new IllegalArgumentException(s"$wrapper doesn't match any inductive cases")
        }
    }
    case Variable(name) => env(Path(name)) match {
      case SimpleObject(value) =>
        if (value != exp)
          throw new IllegalArgumentException(s"Expected $value, but recieved $exp")
        wrapper
      case SimpleFunction(Header(_, params), value) =>
        try {
          val re = value.matchExp(exp, value.getBound.map(_ -> None).toMap, params.map(_.variable -> None).toMap)
            .getOrElse(throw new IllegalStateException(s"Expected substitute of $value, but received $exp"))
          val pars = re._2.map{ case (p, o) => p -> o.getOrElse(throw new IllegalStateException(s"Undefined parameter $p in $exp"))}

          Operation(Variable(name), params.map(d => pars(d.variable)))
        } catch {
          case e: Exception => throw new IllegalArgumentException(s"Expected substitute of $value, but received $exp", e)
        }
    }
    case Lambda(params, value) =>
      try {
        val re = value.matchExp(exp, Map.empty, params.map(_ -> None).toMap)
          .getOrElse(throw new IllegalStateException(s"Expected substitute of $value, but received $exp"))
        val pars = re._2.map { case (p, o) => p -> o.getOrElse(throw new IllegalStateException(s"Undefined parameter $p in $exp")) }

        Operation(Lambda(params, value), params.map(pars))
      } catch {
        case e: Exception => throw new IllegalArgumentException(s"Expected substitute of $value, but received $exp", e)
      }
  }
}
