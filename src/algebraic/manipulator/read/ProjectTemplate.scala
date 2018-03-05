package algebraic.manipulator.read

import algebraic.manipulator.{Graph, Project}

sealed trait ProjectTemplate {
  def dependencies(root: ProjectTemplate, path: List[String]): Set[List[String]]
  def containsFile(path: Traversable[String]) : Boolean
  def apply(): Project
  def apply(root: Project, rootTemplate: ProjectTemplate, parent: Project.Folder, path: List[String]): Unit
}

object ProjectTemplate {
  case class Folder(map: Map[String, ProjectTemplate]) extends ProjectTemplate {
    override def dependencies(root: ProjectTemplate, path: List[String]): Set[List[String]] =
      map.map{ case (k, p) => k -> p.dependencies(root, k :: path)}.values.fold(Set.empty)(_ ++ _).filterNot(_ == path).filterNot(_.tail == path)

    override def containsFile(path: Traversable[String]): Boolean =
      path.nonEmpty && map.contains(path.head) && map(path.head).containsFile(path.tail)

    override def apply(): Project = {
      val folder = new Project.Folder()

      Graph.topologicalSort[String, ProjectTemplate](map, (k, p) => p.dependencies(this, List(k)).filter(_.tail.isEmpty).map(_.head))
        .foreach{case (k, p) => p(folder, this, folder, List(k))}

      folder
    }

    override def apply(root: Project, rootTemplate: ProjectTemplate, parent: Project.Folder, path: List[String]): Unit = {
      val folder = new Project.Folder()
      parent.map += (path.head -> folder)

      Graph.topologicalSort[String, ProjectTemplate](map, (k, p) => p.dependencies(rootTemplate, k :: path).filter(_.tail == path).map(_.head))
        .foreach{case (k, p) => p(root, rootTemplate, folder, k :: path)}
    }
  }
  case class File(file: FileTemplate) extends ProjectTemplate {
    override def dependencies(root: ProjectTemplate, path: List[String]): Set[List[String]] =
      file.dependencies(root).map(_.reverse.tail).map(common(_, path)).filterNot(_ == path)

    override def containsFile(path: Traversable[String]): Boolean = path.isEmpty

    override def apply(): Project = ???

    override def apply(root: Project, rootTemplate: ProjectTemplate, parent: Project.Folder, path: List[String]): Unit =
      parent.map += (path.head -> Project.File(file(root)))

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