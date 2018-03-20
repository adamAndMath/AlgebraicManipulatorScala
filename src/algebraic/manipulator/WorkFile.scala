package algebraic.manipulator

import scala.collection.mutable

class WorkFile(val path: Path) {
  private var using: Map[String,Path] = Map.empty
  private var imports: Set[Path] = Set.empty
  private var elementNames: mutable.MutableList[String] = mutable.MutableList.empty
  private var elements: Map[String, Element] = Map.empty

  class FileEnvironment(private val project: Project, private val file: WorkFile) extends Environment {
    override val path: Path = file.path

    override def apply(path: Path): Element = path match {
      case Path(name) if file.contains(name) => file.get(name)
      case Path(name) =>
        val imp = imports.find(project.getFile(_).contains(name)).getOrElse(throw new IllegalArgumentException(s"$name is not defined in ${file.path}"))
        project.getFile(imp).get(name)
      case _ => project.getFile(toFull(path).dropRight(1)).get(path.last)
    }

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

  def dependencies(project: Project): Set[Path] = {
    env(project).dependencies(elements.values.toList)
  }

  def env(project: Project): Environment = new FileEnvironment(project, this)

  def use(key: String, path: Path): Unit =
    if (using.contains(key))
      throw new IllegalArgumentException
    else
      using += (path.last -> path)

  def importing(path: Path): Unit = imports += path

  def add(name: String, element: Element): Unit = {
    if (contains(name))
      throw new IllegalArgumentException

    elementNames += name
    elements += (name -> element)
  }

  def remove(name: String): Option[Element] = {
    if (!contains(name))
      return None

    val element = elements(name)
    elementNames = elementNames.filterNot(name.equals)
    elements -= name
    Some(element)
  }

  def get(name: String): Element = {
    if (contains(name))
      elements(name)
    else
      throw new IllegalArgumentException(name)
  }

  def contains(name: String): Boolean = elements.contains(name)

  def names: List[String] = elementNames.toList
  def uses: Map[String, Path] = using
}
