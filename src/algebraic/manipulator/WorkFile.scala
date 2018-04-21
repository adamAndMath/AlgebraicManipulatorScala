package algebraic.manipulator

class WorkFile(name: String, parent: Environment, val using: Map[String, Element], val imports: Set[Environment])
  extends Environment.Scope(parent.path :+ name, parent) {

  override def find(path: List[String]): Element = {
    if (contains(path.head)) apply(path)
    else {
      if (using.contains(path.head)) {
        val e = using(path.head)

        if (path.tail.nonEmpty) {
          e match {
            case env: Environment => env(path.tail)
            case _ => e
          }
        } else e
      } else imports.find(_.contains(path.head)).map(_(path)).getOrElse(parent.find(path))
    }
  }

  override def dependencies: Set[String] = super.dependencies.filterNot(d => imports.exists(_.contains(d)))
}
