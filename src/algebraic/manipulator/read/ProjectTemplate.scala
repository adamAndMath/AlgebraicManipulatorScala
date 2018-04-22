package algebraic.manipulator.read

import algebraic.manipulator.{Depending, Element, Environment, Graph}

sealed trait ProjectTemplate extends Depending {
  protected var parent: Option[ProjectTemplate] = None
  def containsFile(path: List[String]): Boolean
  def getFile(path: List[String]): FileTemplate
  def findFile(path: List[String]): FileTemplate
  def apply(): Element
  def apply(name: String, env: Environment.Scope): Element
}

object ProjectTemplate {
  case class Folder(map: Map[String, ProjectTemplate]) extends ProjectTemplate {
    map.values.foreach(_.parent = Some(this))

    override def dependencies: Set[String] =
      map.values.flatMap(_.dependencies).toSet -- map.keys

    override def containsFile(path: List[String]): Boolean =
      path.nonEmpty && map.contains(path.head) && map(path.head).containsFile(path.tail)

    override def getFile(path: List[String]): FileTemplate = map(path.head).getFile(path.tail)

    override def findFile(path: List[String]): FileTemplate =
      if (map.contains(path.head)) getFile(path)
      else parent.getOrElse(throw new IllegalArgumentException(s"no such path as ${path.mkString(".")}")).findFile(path)

    override def apply(): Element = {
      val folder = new Environment.Scope(Nil, Environment.empty)

      Graph.topologicalSort[String, ProjectTemplate](map, (_, p) => p.dependencies.filter(map.contains))
        .foreach{case (k, p) => p(k, folder)}

      folder
    }

    override def apply(name: String, env: Environment.Scope): Element = {
      val scope = env.scope(name)

      Graph.topologicalSort[String, ProjectTemplate](map, (_, p) => p.dependencies.filter(map.contains))
        .foreach{case (k, p) => k -> p(k, scope)}

      scope
    }
  }

  case class File(file: FileTemplate) extends ProjectTemplate {
    override def dependencies: Set[String] =
      file.dependencies(this)

    override def containsFile(path: List[String]): Boolean = path.isEmpty

    override def getFile(path: List[String]): FileTemplate =
      if (path.isEmpty) file
      else throw new IllegalArgumentException(s"${path.mkString(".")} is not reachable from ${file.path.mkString(".")}")

    override def findFile(path: List[String]): FileTemplate =
      if (path.isEmpty) file
      else parent.getOrElse(throw new IllegalArgumentException).findFile(path)

    override def apply(): Element = ???

    override def apply(name: String, env: Environment.Scope): Element = {
      val e = file(name, env)
      env += (name -> e)
      e
    }
  }
}