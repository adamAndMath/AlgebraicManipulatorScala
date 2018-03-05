package algebraic.manipulator

sealed abstract class Project {
  def getFile(path: Traversable[String]) : WorkFile
  def containsFile(path: Traversable[String]) : Boolean
  def dependencies(root: Project, path: List[String]): Set[List[String]]
}

object Project {
  trait Finder{
    def apply(path: List[String]): Identity
    def toFull(path: List[String]): List[String]
  }

  case class File(file: WorkFile) extends Project {
    override def getFile(path: Traversable[String]): WorkFile =
      if (path.isEmpty) file else throw new IllegalArgumentException

    override def containsFile(path: Traversable[String]): Boolean = path.isEmpty

    override def dependencies(root: Project, path: List[String]): Set[List[String]] =
      file.dependencies(root).map(_.reverse.tail).map(common(_, path)).filterNot(_ == path)

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

  class Folder(var map: Map[String, Project] = Map.empty) extends Project {
    override def getFile(path: Traversable[String]): WorkFile =
      if (path.nonEmpty)
        map(path.head).getFile(path.tail)
      else
        throw new IllegalArgumentException

    override def containsFile(path: Traversable[String]): Boolean =
      path.nonEmpty && map.contains(path.head) && map(path.head).containsFile(path.tail)

    override def dependencies(root: Project, path: List[String]): Set[List[String]] =
      map.map{ case (k, p) => k -> p.dependencies(root, k :: path)}.values.fold(Set.empty)(_ ++ _).filterNot(_ == path).filterNot(_.tail == path)
  }
}
