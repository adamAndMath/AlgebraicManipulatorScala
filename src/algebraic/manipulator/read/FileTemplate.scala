package algebraic.manipulator.read

import algebraic.manipulator._

case class FileTemplate(path: List[String], using: Map[String, List[String]], imports: Set[List[String]], identities: List[(String, ElementTemplate)]) {
  def contains(name: String): Boolean = identities.exists(_._1 == name)

  def dependencies(project: ProjectTemplate): Set[String] =
    try {
      identities.flatMap(_._2.dependencies).toSet --
        imports.map(project.findFile).flatMap(_.identities.map(_._1)) --
        using.keys -- identities.map(_._1) ++
        (imports ++ using.values).map(_.head)
    } catch {
      case e: Exception  => throw new IllegalStateException(s"Missing dependencies for ${path.mkString(".")}", e)
    }

  def apply(name: String, env: Environment): Element = {
    try {
      var file: Environment = new WorkFile(env, name, using, imports.flatMap(env.find(_, _.isInstanceOf[Environment]).map(_.asInstanceOf[Environment])))
      identities.foreach{
        case (key, ide) =>
          try {
            file += (key -> ide.apply(file))
          } catch {
            case e: Exception => throw new IllegalStateException(s"Failed to build $key", e)
          }
      }
      file
    } catch {
      case e: RuntimeException => throw new IllegalStateException(s"Exception occurred while building ${path.mkString(".")}", e)
    }
  }
}