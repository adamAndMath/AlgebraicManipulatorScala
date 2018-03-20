package algebraic.manipulator.read

import algebraic.manipulator._

case class FileTemplate(path: Path, using: Map[String, Path], imports: Set[Path], identities: List[(String, ElementTemplate)]) {
  class FileEnvironment(private val project: ProjectTemplate, private val file: FileTemplate) extends Environment {
    override val path: Path = file.path

    override def apply(path: Path): Identity = throw new IllegalStateException

    override def toFull(path: Path): Path = path match {
      case Path(name) if file.contains(name) => file.path + name
      case Path(name) => imports.find(project.getFile(_).contains(name)).map(_ + name).getOrElse(throw new IllegalArgumentException(s"$name is not defined in ${file.path}"))
      case Path(f, name) if using.contains(f) => using(f) + name
      case Path(f, name) =>
        val p = file.path.parent + f
        if (project.containsFile(p))
          p + name
        else
          path
      case _ => path
    }
  }

  def contains(name: String): Boolean = identities.exists(_._1 == name)

  def dependencies(project: ProjectTemplate): Set[Path] = {
    new FileEnvironment(project, this).dependencies(identities.map(_._2))
  }

  def apply(project: Project): WorkFile = {
    try {
      val file = new WorkFile(path)
      using.foreach { case (k, p) => file.use(k, p) }
      imports.foreach(file.importing)
      identities.foreach {
        case (name, ide) =>
          try {
            file.add(name, ide.apply(file.env(project)))
          } catch { case e: Exception => throw new IllegalStateException(s"Failed to build $name", e)}
      }
      file
    } catch {
      case e: RuntimeException => throw new IllegalStateException(s"Exception occurred while building $path", e)
    }
  }
}
