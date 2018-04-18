package algebraic.manipulator

sealed abstract class Project {
  def getFile(path: Path) : WorkFile = getFile(path.toList)
  def containsFile(path: Path) : Boolean = containsFile(path.toList)

  protected def getFile(path: List[String]) : WorkFile
  protected def containsFile(path: List[String]) : Boolean
  def dependencies(root: Project, path: Path): Set[Path]

  def getFiles(sub: Project = this, path: Path = Path.empty): List[WorkFile] = sub match {
    case f: Project.Folder =>
      Graph.topologicalSort[String, Project](f.map, (k, p) => p.dependencies(this, path + k).filter(_.parent == path).map(_.last))
        .flatMap{case (k, p) => getFiles(p, path + k)}
    case Project.File(file) => List(file)
  }
}

object Project {
  case class File(file: WorkFile) extends Project {
    protected override def getFile(path: List[String]): WorkFile =
      if (path.isEmpty) file else throw new IllegalArgumentException

    protected override def containsFile(path: List[String]): Boolean = path.isEmpty

    override def dependencies(root: Project, path: Path): Set[Path] =
      file.dependencies(root).map(path.common).filterNot(_ == path)
  }

  class Folder(var map: Map[String, Project] = Map.empty) extends Project {
    protected override def getFile(path: List[String]): WorkFile =
      if (path.nonEmpty)
        map(path.head).getFile(path.tail)
      else
        throw new IllegalArgumentException

    protected override def containsFile(path: List[String]): Boolean =
      path.nonEmpty && map.contains(path.head) && map(path.head).containsFile(path.tail)

    override def dependencies(root: Project, path: Path): Set[Path] =
      map.map{ case (k, p) => k -> p.dependencies(root, path + k)}.values.fold(Set.empty)(_ ++ _).filterNot(_ == path).filterNot(_.parent == path)
  }
}
