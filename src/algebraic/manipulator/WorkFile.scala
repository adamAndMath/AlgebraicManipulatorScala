package algebraic.manipulator

import scala.collection.mutable

class WorkFile(val path: List[String]) {
  private var using: Map[String,List[String]] = Map.empty
  private var proofNames: mutable.MutableList[String] = mutable.MutableList.empty
  private var proofs: Map[String, Identity] = Map.empty

  class Finder(private val project: Project, private val file: WorkFile) extends Project.Finder {
    override def apply(path: List[String]): Identity = {
      val size = path.size

      if (size == 1)
        file.get(path.head)
      else
        project.getFile(toFull(path).dropRight(1)).get(path.last)
    }

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

  def dependencies(project: Project): Set[List[String]] = {
    val finder = new Finder(project, this)
    proofs.values.map(_.dependencies(finder)).fold(Set.empty)(_ ++ _)
  }

  def find(project: Project): Project.Finder = new Finder(project, this)

  def use(key: String, path: List[String]): Unit =
    if (using.contains(key))
      throw new IllegalArgumentException
    else
      using += (path.last -> path)

  def add(name: String, proof: Identity): Unit = {
    if (proofs contains name)
      throw new IllegalArgumentException

    proofNames += name
    proofs += (name -> proof)
  }

  def remove(name: String): Option[Identity] = {
    if (!proofs.contains(name))
      return None

    val proof = proofs(name)
    proofs -= name
    Some(proof)
  }

  def get(name: String): Identity = {
    if (proofs contains name)
      proofs(name)
    else
      throw new IllegalArgumentException
  }

  def names: List[String] = proofNames.toList
  def uses: Map[String, List[String]] = using
}
