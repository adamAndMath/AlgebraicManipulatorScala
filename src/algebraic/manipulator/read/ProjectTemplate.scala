package algebraic.manipulator.read

import algebraic.manipulator.{Graph, Project}

sealed trait ProjectTemplate {
  def dependencies(root: Project, path: List[String]): Set[List[String]]
  def apply(): Project
  def apply(root: Project, parent: Project.Folder, path: List[String]): Unit
}

object ProjectTemplate {
  case class Folder(map: Map[String, ProjectTemplate]) extends ProjectTemplate {
    override def dependencies(root: Project, path: List[String]): Set[List[String]] =
      map.map{ case (k, p) => k -> p.dependencies(root, k :: path)}.values.fold(Set.empty)(_ ++ _).filterNot(_ == path).filterNot(_.tail == path)

    override def apply(): Project = {
      val folder = new Project.Folder()

      Graph.topologicalSort[String, ProjectTemplate](map, (k, p) => p.dependencies(folder, List(k)).filter(_.tail.isEmpty).map(_.head))
        .foreach{case (k, p) => p(folder, folder, List(k))}

      folder
    }

    override def apply(root: Project, parent: Project.Folder, path: List[String]): Unit = {
      val folder = new Project.Folder()
      parent.map += (path.head -> folder)

      Graph.topologicalSort[String, ProjectTemplate](map, (k, p) => p.dependencies(root, k :: path).filter(_.tail == path).map(_.head))
        .foreach{case (k, p) => p(root, folder, k :: path)}
    }
  }
  case class File(file: FileTemplate) extends ProjectTemplate {
    override def dependencies(root: Project, path: List[String]): Set[List[String]] =
      file.dependencies(root).map(_.reverse).map(common(_, path)).filterNot(_ == path)

    override def apply(): Project = ???

    override def apply(root: Project, parent: Project.Folder, path: List[String]): Unit =
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
      val ns = set.dropWhile{case (e1, e2) => e1.tail == e2.tail}
      if (ns.isEmpty)
        set.map(_._1)
      else
        common(ns.dropWhile{case (e1, e2) => e1.tail != e2.tail})
    }
  }
}