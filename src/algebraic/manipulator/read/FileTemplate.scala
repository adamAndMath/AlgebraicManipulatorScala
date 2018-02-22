package algebraic.manipulator.read

import algebraic.manipulator._

case class FileTemplate(path: List[String], using: Map[String, List[String]], identities: List[(String, IdentityTemplate)]) {
  class Finder(private val project: Project, private val file: FileTemplate) extends Project.Finder {
    override def apply(path: List[String]): Identity = throw new IllegalStateException

    override def toFull(path: List[String]): List[String] = {
      val size = path.size

      if (size == 1)
        file.path ++ path
      else if (size == 2 && using.contains(path.head))
        using(path.head) ++ path.tail
      else
        path
    }
  }

  def dependencies(project: Project): Set[List[String]] = {
    val finder = new Finder(project, this)
    identities.map(_._2.dependencies(finder)).fold(Set.empty)(_ ++ _)
  }

  def apply(project: Project): WorkFile = {
    try {
      val file = new WorkFile(path)
      using.foreach { case (k, p) => file.use(k, p) }
      identities.foreach { case (name, ide) => file.add(name, ide.apply(file.find(project))) }
      file
    } catch {
      case e: RuntimeException => throw new IllegalStateException(s"Exception occurred while building ${path.mkString(".")}", e)
    }
  }
}
