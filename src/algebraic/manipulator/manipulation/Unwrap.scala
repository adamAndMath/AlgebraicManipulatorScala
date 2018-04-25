package algebraic.manipulator.manipulation

import algebraic.manipulator._

case class Unwrap(positions: Tree) extends PathManipulation(positions) {
  override def dependencies: Set[String] = Set.empty

  override def replace(env: Environment, exp: Exp): Exp = exp match {
    case Variable(name) => env.find(List(name), _.isInstanceOf[Wrapable]).get.asInstanceOf[Wrapable].unwrap(exp)
    case Operation(Variable(name), _) => env.find(List(name), _.isInstanceOf[Wrapable]).get.asInstanceOf[Wrapable].unwrap(exp)
    case Operation(Lambda(params, value), args) =>
      if (params.length != args.length)
        throw new IllegalArgumentException(s"Expected ${params.length} arguments, but received ${args.length}")

      value.set((params zip args).toMap)
    case Lambda(params, Operation(op, args)) =>
      if (params != args)
        throw new IllegalArgumentException(s"Expected the arguments $params, but received $args")

      op
  }
}
