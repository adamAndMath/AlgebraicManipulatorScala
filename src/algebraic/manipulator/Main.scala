package algebraic.manipulator

import java.nio.file.{Files, Paths}

import algebraic.manipulator.Latex.LatexWriter
import algebraic.manipulator.read.ProofReader

object Main {
  def main(args: Array[String]): Unit = {
    val projectTemplate = ProofReader.readProject(Paths.get(args(0)))
    println(projectTemplate)
    val project = projectTemplate()
    println(project)
    Files.write(Paths.get(args(1)), LatexWriter(project, "Simple Proofs", "Mathias Adam Møller").getBytes)
  }
}
