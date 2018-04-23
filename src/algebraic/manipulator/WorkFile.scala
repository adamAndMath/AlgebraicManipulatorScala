package algebraic.manipulator

class WorkFile(parent: Environment, name: String, val using: Map[String, List[String]], val imports: Set[Environment]) extends Environment {
  override val path: List[String] = parent.path :+ name
  override val bound: Set[String] = imports.flatMap(_.names) ++ using.keys

  override def dependencies: Set[String] =
    imports.map(_.path).map(p => p.drop((p zip path).takeWhile(e => e._1==e._2).length).head) ++ using.values.map(_.head)

  override def find(path: List[String]): Element =
    using.get(path.head).map(parent.find).map(e => {
      if (path.tail.nonEmpty) {
        e match {
          case env: Environment => env(path.tail)
          case _ => e
        }
      } else e
    }).getOrElse(imports.find(_.contains(path.head)).map(_(path)).getOrElse(parent.find(path)))

  override def toFull(path: List[String]): List[String] =
    using.get(path.head).map(_ ++ path.tail).getOrElse(imports.find(_.contains(path.head)).map(_.path ++ path).getOrElse(parent.toFull(path)))
}
