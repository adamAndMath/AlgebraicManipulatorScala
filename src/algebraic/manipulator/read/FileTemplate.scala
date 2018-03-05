package algebraic.manipulator.read

import algebraic.manipulator._

case class FileTemplate(path: List[String], using: Map[String, List[String]], identities: List[(String, IdentityTemplate)]) {
  class Finder(private val project: ProjectTemplate, private val file: FileTemplate) extends Project.Finder {
    override def apply(path: List[String]): Identity = throw new IllegalStateException

    override def toFull(path: List[String]): List[String] = {
      val size = path.length

      if (size == 1)
        file.path ++ path
      else if (size == 2) {
        if (using.contains(path.head))
          using(path.head) ++ path.tail
        else {
          val p = file.path.dropRight(1) ++ path.take(1)
          if (project.containsFile(p))
            p ++ path.tail
          else
            path
        }
      } else path
    }
  }

  def dependencies(project: ProjectTemplate): Set[List[String]] = {
    val finder = new Finder(project, this)
    identities.map(_._2.dependencies(finder)).fold(Set.empty)(_ ++ _)
  }

  def apply(project: Project): WorkFile = {
    try {
      val file = new WorkFile(path)
      using.foreach { case (k, p) => file.use(k, p) }
      identities.foreach {
        case (name, ide) =>
          try {
            file.add(name, ide.apply(file.find(project)))
          } catch { case e: Exception => throw new IllegalStateException(s"Failed to build $name", e)}
      }
      file
    } catch {
      case e: RuntimeException => throw new IllegalStateException(s"Exception occurred while building ${path.mkString(".")}", e)
    }
  }
}
