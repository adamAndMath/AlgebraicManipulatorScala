package algebraic.manipulator

import java.nio.file.{Files, Paths}

import algebraic.manipulator.latex._
import algebraic.manipulator.read.ProofReader

object Main {
  def main(args: Array[String]): Unit = {
    LatexDefault.setup()
    val projectTemplate = ProofReader.readProject(Paths.get(args(0)))
    println(projectTemplate)
    val project = projectTemplate()
    println(project)
    args.drop(4).foreach(p => LatexWriter.expWriter ++= ExpWriter(Paths.get(p)))
    Files.write(Paths.get(args(1)), LatexWriter(project, args(2), args(3)).getBytes)
  }
}
