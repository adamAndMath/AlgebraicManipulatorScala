package algebraic.manipulator

import java.nio.file.Paths

import algebraic.manipulator.read.ProofReader

object Main {
  def main(args: Array[String]): Unit = {
    val projectTemplate = ProofReader.readProject(Paths.get(args(0)))
    println(projectTemplate)
    val project = projectTemplate()
    println(project)
  }
}
