package algebraic.manipulator

import java.nio.file.{Files, Paths}

import algebraic.manipulator.latex._
import algebraic.manipulator.options.Options
import algebraic.manipulator.read.ProofReader

object Main {
  def main(args: Array[String]): Unit = {
    var projects = List.empty[String]
    var output = ""
    var safe = true

    LatexWriter.registerOptions()
    Options += ("p", args => projects ++= args)
    Options += ("out", 1, args => output = args.head)
    Options += ("safe", 1, args => safe = Options.bool(args.head))

    Options.read(args.toList)

    val projectTemplate = ProofReader.readProject(Paths.get(projects.head))
    val project = projectTemplate()

    if (safe) {
      val errs = project.getFiles().flatMap(file => {
        val env = file.env(project)
        file.names.flatMap(name => file.get(name).validate(env).map(v => s"${file.path}: $name: $v"))
      })

      if (errs.nonEmpty)
        throw new IllegalStateException("Validation errors:\n"+errs.mkString("\n"))
    }

    Files.write(Paths.get(output), LatexWriter(project).getBytes)
  }
}
