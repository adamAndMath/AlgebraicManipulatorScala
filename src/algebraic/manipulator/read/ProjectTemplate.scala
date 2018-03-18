package algebraic.manipulator.read

import algebraic.manipulator.{Graph, Path, Project}

sealed trait ProjectTemplate {
  def containsFile(path: Path): Boolean = containsFile(path.toList)
  def getFile(path: Path): FileTemplate = getFile(path.toList)

  def dependencies(root: ProjectTemplate, path: Path): Set[Path]
  protected def containsFile(path: List[String]): Boolean
  protected def getFile(path: List[String]): FileTemplate
  def apply(): Project
  def apply(root: Project, rootTemplate: ProjectTemplate, parent: Project.Folder, path: Path): Unit
}

object ProjectTemplate {
  case class Folder(map: Map[String, ProjectTemplate]) extends ProjectTemplate {
    override def dependencies(root: ProjectTemplate, path: Path): Set[Path] =
      map.map{ case (k, p) => k -> p.dependencies(root, path + k)}.values.fold(Set.empty)(_ ++ _).filterNot(_ == path).filterNot(_.parent == path)

    protected override def containsFile(path: List[String]): Boolean =
      path.nonEmpty && map.contains(path.head) && map(path.head).containsFile(path.tail)

    protected override def getFile(path: List[String]): FileTemplate = map(path.head).getFile(path.tail)

    override def apply(): Project = {
      val folder = new Project.Folder()

      Graph.topologicalSort[String, ProjectTemplate](map, (k, p) => p.dependencies(this, Path(k)).filter(_.parent.isEmpty).map(_.last))
        .foreach{case (k, p) => p(folder, this, folder, Path(k))}

      folder
    }

    override def apply(root: Project, rootTemplate: ProjectTemplate, parent: Project.Folder, path: Path): Unit = {
      val folder = new Project.Folder()
      parent.map += (path.last -> folder)

      Graph.topologicalSort[String, ProjectTemplate](map, (k, p) => p.dependencies(rootTemplate, path + k).filter(_.parent == path).map(_.last))
        .foreach{case (k, p) => p(root, rootTemplate, folder, path + k)}
    }
  }
  case class File(file: FileTemplate) extends ProjectTemplate {
    override def dependencies(root: ProjectTemplate, path: Path): Set[Path] =
      file.dependencies(root).map(_.parent).map(path.common).filterNot(_ == path)

    protected override def containsFile(path: List[String]): Boolean = path.isEmpty

    protected override def getFile(path: List[String]): FileTemplate =
      if (path.isEmpty) file
      else throw new IllegalArgumentException

    override def apply(): Project = ???

    override def apply(root: Project, rootTemplate: ProjectTemplate, parent: Project.Folder, path: Path): Unit =
      parent.map += (path.last -> Project.File(file(root)))

    private def common(a: List[String], b: List[String]): List[String] = common({
      val as = a.size
      val bs = b.size

      if (as == bs)
        a zip b
      else if (as > bs)
        a.drop(as - bs) zip b
      else
        a zip b.drop(bs - as)
    })

    private def common(set: List[(String, String)]): List[String] = {
      val ns = set.tail.takeWhile{case (e1, e2) => e1 == e2}.length
      if (ns+1 == set.length)
        set.map(_._1)
      else
        common(set.drop(ns + set.drop(ns+1).takeWhile{case (e1, e2) => e1 != e2}.length))
    }
  }
}