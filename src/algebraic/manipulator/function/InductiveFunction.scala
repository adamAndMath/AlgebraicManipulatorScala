package algebraic.manipulator.function

import algebraic.manipulator._
import algebraic.manipulator.manipulation.Wrapable
import algebraic.manipulator.specifiers.{HeadMatch, Header, TypeHeader}

case class InductiveFunction(header: Header, base: InductiveBase, steps: List[InductiveStep]) extends FunctionElement with Wrapable {
  override def typeHeader: TypeHeader = header.toType

  override def validate(name: String, env: Environment): Traversable[(List[String], String)] = None

  override def wrap(template: Exp, exp: Exp): Exp = template match {
    case Operation(Variable(_), parameters) =>
      val parMap = header.parameters.map(_.variable).zip(parameters).toMap
      val inductive = parMap(base.inductive)

      if (base.value == inductive) {
        val headMatch = HeadMatch(Map.empty, base.exp.getBound.map(_ -> None).toMap, (parMap - base.inductive).map(p => Definition(AnyType, p._1) -> Some(p._2)))
        val re = base.exp.matchExp(exp, headMatch)
          .getOrElse(throw new IllegalArgumentException(s"Expected ${base.exp.set(parMap)}, but received $exp"))
        re.dummies.foreach{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer dummy $p"))}
        re.parameters.foreach{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer parameter $p"))}
        template
      } else {
        for (step <- steps) {
          val headMatch = HeadMatch(Map.empty, step.step.getBound.map(_ -> None).toMap, step.params.map(_ -> None).toMap + (Definition(AnyType, base.inductive) -> None))
          val indRe = step.step.matchExp(inductive, headMatch)
          if (indRe.exists(r => (r.dummies.values ++ r.parameters.values).forall(_.isDefined))) {
            val indHead = HeadMatch(Map.empty, step.exp.getBound.map(_ -> None).toMap, parMap.map(p => Definition(AnyType, p._1) -> Some(p._2)) ++ indRe.get.parameters)
            val re = step.exp.matchExp(exp, indHead)
              .getOrElse(throw new IllegalArgumentException(s"Expected substitute of ${step.exp.set(indRe.get.parameters.map(p => p._1.variable -> p._2.get))}, but received $exp"))
            re.dummies.foreach{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer dummy $p"))}
            re.parameters.foreach{case (p, v) => p -> v.getOrElse(throw new IllegalArgumentException(s"Can't infer parameter $p"))}
            return template
          }
        }

        throw new IllegalArgumentException(s"$template doesn't match any inductive cases")
      }
    case _ => throw new IllegalArgumentException(s"Unable to decide function part $template")
  }

  override def unwrap(exp: Exp): Exp = exp match {
    case Operation(Variable(f), parameters) =>
      if (header.parameters.length != parameters.length)
        throw new IllegalArgumentException(s"Expected ${header.parameters.length} arguments, but received ${parameters.length}")

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

      throw new IllegalArgumentException(s"No case in $f matches $inductive")
    case _ => throw new IllegalArgumentException(s"Functions should only be unwrapped with parameters")
  }
}

case class InductiveBase(inductive: Variable, value: Exp, exp: Exp) extends Depending {
  override def dependencies: Set[String] = value.dependencies ++ exp.dependencies
}

case class InductiveStep(params: List[Definition], step: Exp, exp: Exp) extends Depending {
  override def dependencies: Set[String] =
    Header(Nil, Nil, params).scope(step.dependencies ++ exp.dependencies)
}