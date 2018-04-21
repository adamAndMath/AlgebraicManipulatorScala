package algebraic.manipulator

class WorkFile(name: String, parent: Environment) extends Environment.Scope(parent.path :+ name, parent) {
  private var using: Map[String,List[String]] = Map.empty
  private var imports: Set[Environment] = Set.empty

  override def find(path: List[String]): Element = {
    if (contains(path.head)) apply(path)
    else  {
      if (using.contains(path.head)) {
        val e = find(using(path.head))

        if (path.tail.nonEmpty) {
          e match {
            case env: Environment => env(path.tail)
            case _ => e
          }
        } else e
      } else imports.find(_.contains(path.head)).map(_(path)).getOrElse(parent.find(path))
    }
  }

  override def dependencies: Set[String] =
    super.dependencies.filterNot(d => imports.exists(_.contains(d)))

  def use(key: String, path: List[String]): Unit =
    if (using.contains(key))
      throw new IllegalArgumentException
    else
      using += (key -> path)

  def importing(path: List[String]): Unit = imports += find(path).asInstanceOf[Environment]
  def uses: Map[String, List[String]] = using
}
