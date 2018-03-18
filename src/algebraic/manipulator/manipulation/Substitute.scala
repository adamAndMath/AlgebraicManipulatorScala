package algebraic.manipulator.manipulation

import algebraic.manipulator._

case class Substitute(positions: Tree, path: Path, from: Int, to: Int, dummies: List[Variable], parameters: Option[List[Option[Exp]]]) extends PathManipulation(positions) {
  override def dependencies(finder: Project.Finder): Set[Path] = Set(finder.toFull(path))

  override def replace(finder: Project.Finder, exp: Exp): Exp = {
    val identity = finder(path).asInstanceOf[Identity]
    val params: List[Option[Exp]] = parameters.getOrElse(List.fill[Option[Exp]](identity.header.parameters.length)(None))

    if (this.dummies.length != identity.header.dummies.length)
      throw new IllegalStateException("Dummy count does't match referred work")

    if (params.length != identity.header.parameters.length)
      throw new IllegalStateException("Parameter count does't match referred work")

    val fromExp = identity.result(from)
    val dummy = fromExp.getBound
    val tree: PathTree[Variable] = fromExp.tree.filter(!dummy.contains(_))

    var pars: Map[Variable, Exp] = (identity.header.parameters.map(_.variable) zip params).toMap.filter{case (_, p) => p.nonEmpty}.mapValues(_.get)

    if (tree.nonEmpty) {
      try {
        exp.get(tree).foreach{ case (p, e) =>
          if (!pars.contains(p))
            pars += (p -> e)
          else if (pars(p) != e)
            throw new IllegalStateException(s"Expected parameter ${pars(p)}, but received $e")
        }
      } catch {
        case e: Exception => throw new IllegalStateException(s"Expected substitute of $fromExp, but received $exp", e)
      }
    }

    for (par <- identity.header.parameters.map(_.variable))
      if (!pars.contains(par))
        throw new IllegalStateException(s"Undefined parameter $par ${exp.get(tree)}")

    val fromSet = fromExp.setAll((identity.header.dummies zip dummies).toMap, pars)

    if (fromSet != exp)
      throw new IllegalStateException(s"Expected $fromSet, but received $exp")

    identity.result(to).setAll((identity.header.dummies zip dummies).toMap, pars)
  }
}
