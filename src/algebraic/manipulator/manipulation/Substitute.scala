package algebraic.manipulator.manipulation

import algebraic.manipulator._

case class Substitute(positions: Tree, path: List[String], from: Int, to: Int, dummies: List[Variable], parameters: Option[List[Option[Exp]]]) extends PathManipulation(positions) {
  override def dependencies(finder: Project.Finder): Set[List[String]] = Set(finder.toFull(path))

  override def replace(finder: Project.Finder, exp: Exp): Exp = {
    val identity = finder(path)
    val params: List[Option[Exp]] = parameters.getOrElse(List.fill[Option[Exp]](identity.header.parameters.length)(None))

    if (this.dummies.length != identity.header.dummies.length)
      throw new IllegalStateException("Dummy count does't match referred work")

    if (params.length != identity.header.parameters.length)
      throw new IllegalStateException("Parameter count does't match referred work")

    val fromExp = identity.result(from)
    val dummy = fromExp.getBound
    val tree: PathTree[Variable] = fromExp.tree(v => v).filter(!dummy.contains(_))

    var pars: Map[Variable, Exp] = (identity.header.parameters.map(_.variable()) zip params).toMap.filter{case (_, p) => p.nonEmpty}.mapValues(_.get)

    if (tree.nonEmpty) {
      try {
        exp.get(tree).foreach{ case (e, p) =>
          if (!pars.contains(p))
            pars += (p -> e)
          else if (pars(p) == e)
            throw new IllegalStateException(s"Expected parameter ${pars(p)}, but received $e")
        }
      } catch {
        case _: Exception => throw new IllegalStateException(s"Expected substitute of $fromExp, but received $exp")
      }
    }

    val fromSet = fromExp.setAll((identity.header.dummies zip dummies).toMap, pars)

    if (fromSet != exp)
      throw new IllegalStateException(s"Expected $fromSet, but received $exp")

    identity.result(to).setAll((identity.header.dummies zip dummies).toMap, pars)
  }
}
