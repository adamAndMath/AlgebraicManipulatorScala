package algebraic.manipulator.manipulation
import algebraic.manipulator._
import algebraic.manipulator.function.{InductiveFunction, SimpleFunction}
import algebraic.manipulator.objects.SimpleObject
import algebraic.manipulator.specifiers.HeadMatch

case class Wrap(wrapper: Exp, positions: Tree) extends PathManipulation(positions) {
  override def dependencies: Set[String] = wrapper.dependencies

  override def replace(env: Environment, exp: Exp): Exp = wrapper match {
    case Operation(Variable(name), parameters) => env.find(List(name)) match {
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
          val headMatch = HeadMatch(Map.empty, base.exp.getBound.map(_ -> None).toMap, (parMap - base.inductive).map(p => Definition(AnyType, p._1) -> Some(p._2)))
          val re = base.exp.matchExp(exp, headMatch)
            .getOrElse(throw new IllegalArgumentException(s"Expected ${base.exp.set(parMap)}, but received $exp"))
          val dums = re.dummies.map{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer dummy $p"))}
          val pars = re.parameters.map{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer parameter $p"))}
          wrapper
        } else {
          for (step <- steps) {
            val headMatch = HeadMatch(Map.empty, step.step.getBound.map(_ -> None).toMap, step.params.map(_ -> None).toMap + (Definition(AnyType, base.inductive) -> None))
            val indRe = step.step.matchExp(inductive, headMatch)
            if (indRe.exists(r => (r.dummies.values ++ r.parameters.values).forall(_.isDefined))) {
              val indHead = HeadMatch(Map.empty, step.exp.getBound.map(_ -> None).toMap, parMap.map(p => Definition(AnyType, p._1) -> Some(p._2)) ++ indRe.get.parameters)
              val re = step.exp.matchExp(exp, indHead)
                .getOrElse(throw new IllegalArgumentException(s"Expected substitute of ${step.exp.set(indRe.get.parameters.map(p => p._1.variable -> p._2.get))}, but received $exp"))
              val dums = re.dummies.map{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer dummy $p"))}
              val pars = re.parameters.map{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer parameter $p"))}
              return wrapper
            }
          }

          throw new IllegalArgumentException(s"$wrapper doesn't match any inductive cases")
        }
    }
    case Variable(name) => env.find(List(name)) match {
      case SimpleObject(value) =>
        if (value != exp)
          throw new IllegalArgumentException(s"Expected $value, but recieved $exp")
        wrapper
      case SimpleFunction(header, value) =>
        try {
          val re = value.matchExp(exp, header.toMatch)
            .getOrElse(throw new IllegalStateException(s"Expected substitute of $value, but received $exp"))
          val pars = re.parameters.map{ case (p, o) => p -> o.getOrElse(throw new IllegalStateException(s"Undefined parameter $p in $exp"))}

          Operation(Variable(name), header.parameters.map(pars))
        } catch {
          case e: Exception => throw new IllegalArgumentException(s"Expected substitute of $value, but received $exp", e)
        }
    }
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
