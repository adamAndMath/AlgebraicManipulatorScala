package algebraic.manipulator.latex

import java.nio.file.Paths
import java.time.LocalDate
import java.time.format.TextStyle
import java.util.Locale

import algebraic.manipulator._
import algebraic.manipulator.function._
import algebraic.manipulator.manipulation._
import algebraic.manipulator.objects._
import algebraic.manipulator.options.Options
import algebraic.manipulator.structure._

object LatexWriter {
  var expWriter: ExpWriter = ExpWriter(Map.empty, Map.empty, Nil)
  var colors = List("red", "blue", "olive", "orange", "yellow")
  var title = ""
  var author = ""
  var hyperlink = true
  var definitions = true
  var color = true

  sealed trait ElementLocation
  case object AssumptionLocation extends ElementLocation
  case object DefinitionLocation extends ElementLocation
  case object ProofLocation extends ElementLocation
  case object IgnoredLocation extends ElementLocation

  def registerOptions(): Unit = {
    Options += ("title", 1, args => title = args.head)
    Options += ("author", 1, args => author = args.head)
    Options += ("link", 1, args => hyperlink = Options.bool(args.head))
    Options += ("defs", 1, args => definitions = Options.bool(args.head))
    Options += ("color", 1, args => color = Options.bool(args.head))
    Options += ("colors", args => colors = args)
    Options += ("layout", args => expWriter = (expWriter /: args.map(a => ExpWriter(Paths.get(a))))(_ ++ _))
  }

  def apply(project: Project): String = {
    val date = LocalDate.now()
    "\\documentclass{report}\n" +
      "\\usepackage[utf8]{inputenc}\n" +
      "\\usepackage{hyperref}\n" +
      "\\usepackage{amssymb}\n" +
      "\\usepackage{amsmath}\n" +
      "\\usepackage{xcolor}\n\n" +
      s"\\title{$title}\n" +
      s"\\author{$author}\n" +
      s"\\date{${date.getMonth.getDisplayName(TextStyle.FULL, Locale.ENGLISH)} ${date.getYear}}\n\n\n" +
      "\\begin{document}\n\n" +
      "\\maketitle\n" +
      "\\tableofcontents\n\n" +
      project.getFiles().map(writeFile(project, _)).mkString("\n") +
      "\n\\end{document}"
  }

  def writeFile(project: Project, file: WorkFile): String = {
    val env = file.env(project)

    val assumptions = file.names.filter(n => getLocation(file.get(n)) == AssumptionLocation)

    s"\\chapter{${file.path.last}}\n" +
      (file.names.filter(n => getLocation(file.get(n)) == DefinitionLocation).map(name =>
        writeElementDefinition(name, file.get(name))
      ) ++
      {
        if (assumptions.nonEmpty)
          "Given the following assumptions:" ::
            assumptions.map(name =>
              s"\\label{${(file.path + name).mkString(":")}}\n" +
                writeAssumption(file.get(name))
            )
        else Nil
      }).mkString("\\\\\n") + "\n" +
      file.names
        .filter(n => getLocation(file.get(n)) == ProofLocation)
        .map(name => {
        s"\\section{$name}\n" +
          s"\\label{${(file.path + name).mkString(":")}}\n" +
          writeElement(env, file.get(name))
      }).mkString
  }

  def getLocation(element: Element): ElementLocation = element match {
    case _: Assumption => AssumptionLocation
    case _: AssumedFunction => IgnoredLocation
    case SimpleStructure => IgnoredLocation
    case InductiveStructure(_, _) => DefinitionLocation
    case _: ObjectElement => DefinitionLocation
    case _: FunctionElement => DefinitionLocation
    case _: Identity => ProofLocation
  }

  def writeElementDefinition(name: String, element: Element): String = element match {
    case AssumedObject => s"Let $name be an object"
    case SimpleObject(exp) => s"Let $$${writeExp(Variable(name))} = ${writeExp(exp)}$$"
    case InductiveStructure(base, steps) =>
      val typeOut = writeType(SimpleType(name))
      s"Let $$$typeOut$$ be the smallest set that satisfies " +
        s"$$${writeDefinition(Header(Nil, base.params))}${writeExp(base.exp)} \\in $typeOut$$" +
        steps.map(step => s" and $$${writeDefinition(Header(Nil, Definition(SimpleType(name), step.v.name) :: step.params))}${writeExp(step.exp)} \\in $typeOut$$").mkString
    case SimpleFunction(header, exp) => s"Let $$${writeExp(Operation(Variable(name), header.parameters.map(_.variable)))} = ${writeExp(exp)}$$"
    case InductiveFunction(header, base, steps) =>
      s"Let $$${writeExp(Operation(Variable(name), header.parameters.map(_.variable)))} = \n" +
        "  \\begin{cases}\n" +
        s"    ${base.exp} & \\quad ${base.inductive} = ${base.value}" +
        s"${steps.map(s => s"\\\\\n    ${s.exp} & \\quad ${s.step}").mkString}" +
        "\n  \\end{cases}$"
  }

  def writeAssumption(element: Element): String = element match {
    case a: Assumption => s"$$${writeDefinition(a.header)}${a.result.map(writeExp(_)).mkString("=")}$$"
  }

  def writeElement(env: Environment, element: Element): String = element match {
    case p: Proof =>
      var exps = List.fill(p.count)(p.origin)
      var res = ""

      var backColor = PathTree.empty[String]
      var textColor = PathTree.empty[String]

      for (manipulation <- p.manipulations) {
        backColor = getInputColors(env, exps, manipulation)
        res += "$$" + (exps.indices zip exps).map{case (i, exp) => writeExp(exp, textColor(i), backColor(i))}.mkString("=") + "$$\n"

        exps = manipulation(env, exps)
        textColor = getOutputColors(env, exps, manipulation)

        res += s"{\\color{gray}${writeManipulation(env, manipulation)}}\n"
      }

      backColor = PathTree.empty
      res += "$$" + (exps.indices zip exps).map{case (i, exp) => writeExp(exp, textColor(i), backColor(i))}.mkString("=") + "$$\n"

      res
    case p: InductionProof => "Proof by induction\n" +
      s"\\subsection{$$${p.inductives.toList.map{case (v, exp) => s"$v = ${writeExp(exp)}"}.mkString(",")}$$}\n${writeElement(env, p.base)}\n" +
      p.inductives.keySet.map(v => p(v).map(i => s"\\subsection{$v'=$$${writeExp(i.exp)}$$}\n${writeElement(i.bindStep(env), i.proof)}").mkString("\n")).mkString("\n")
    case p: AssumedProof =>
      var exps = p.origin
      var res = ""

      var backColor = PathTree.empty[String]
      var textColor = PathTree.empty[String]

      for (manipulation <- p.manipulations) {
        backColor = getInputColors(env, exps, manipulation)
        res += "$$" + (exps.indices zip exps).map{case (i, exp) => writeExp(exp, textColor(i), backColor(i))}.mkString("=") + "$$\n"

        exps = manipulation(env, exps)
        textColor = getOutputColors(env, exps, manipulation)

        res += s"{\\color{gray}${writeManipulation(env, manipulation)}}\n"
      }

      backColor = PathTree.empty
      res += "$$" + (exps.indices zip exps).map{case (i, exp) => writeExp(exp, textColor(i), backColor(i))}.mkString("=") + "$$\n"

      res
  }

  def getInputColors(env: Environment, exps: List[Exp], manipulation: Manipulation): PathTree[String] = manipulation match {
    case Call(_, _) => PathTree.empty
    case Substitute(positions, path, from, _, _, _) =>
      val identity = env(path).asInstanceOf[Identity]
      val parameters = identity.header.parameters.map(_.variable)
      positions :: identity.result(from).tree.filter(parameters.contains).map(parameters.indexOf(_)).map(colors)
    case Rename(positions, _, _) => positions :> colors.head
    case Wrap(_, positions) => positions :> colors.head
    case Unwrap(positions) => positions :> colors.head
  }

  def getOutputColors(env: Environment, exps: List[Exp], manipulation: Manipulation): PathTree[String] = manipulation match {
    case Call(_, _) => PathTree.empty
    case Substitute(positions, path, _, to, _, _) =>
      val identity = env(path).asInstanceOf[Identity]
      val parameters = identity.header.parameters.map(_.variable)
      positions :: identity.result(to).tree.filter(parameters.contains).map(parameters.indexOf(_)).map(colors)
    case Rename(positions, _, _) => positions :> colors.head
    case Wrap(_, positions) => positions :> colors.head
    case Unwrap(positions) => positions :> colors.head
  }

  def writeManipulation(env: Environment, manipulation: Manipulation): String = manipulation match {
    case Call(temp, exp) => s"Call $$${writeExp(exp, exp.tree.filter(_ == temp).map(_ => colors.head))}$$"
    case Substitute(_, path, from, to, _, _) =>
      val identity = env(path).asInstanceOf[Identity]
      writeIdentityReference(env.toFull(path), identity.header, List(from, to).map(identity.result))
    case Rename(_, from, to) => s"Renaming $from to $to"
    case Wrap(Variable(name), _) => env(Path(name)) match {
      case SimpleObject(exp) => writeIdentityReference(env.toFull(Path(name)), Header(Nil, Nil), List(Variable(name), exp))
      case SimpleFunction(header, exp) => writeIdentityReference(env.toFull(Path(name)), header, List(Operation(Variable(name), header.parameters.map(_.variable)), exp))
    }
    case Wrap(Lambda(params, e), _) =>
      s"Wrapping $$${params.map(writeExp(_)).mkString("(", ", ", ")")} \\rightarrow ${writeExp(e)}$$"
    case Wrap(_, _) => "Wrapping"
    case Unwrap(_) => "Unwrapping"
  }

  def writeIdentityReference(path: Path, header: Header, exps: List[Exp]): String = {
    val parameters = header.parameters.map(_.variable)
    val equation = exps.map(e => writeExp(e, e.tree.filter(parameters.contains).map(v => colors(parameters.indexOf(v)))))
    val str = s"$$${writeDefinition(header, v => Some(colors(parameters.indexOf(v))))}${equation.mkString("=")}$$"

    if (hyperlink) s"\\hyperref[${path.mkString(":")}]{$str}"
    else str
  }

  def writeDefinition(header: Header, colors: Variable => Option[String] = _ => None): String =
    if (!definitions) ""
    else header.parameters match {
      case Nil => ""
      case par :: Nil =>
        s"\\forall ${colorText(colors(par.variable), par.name)} \\in ${writeType(par.varType)}: "
      case Definition(varType, _) :: tail if tail.forall(_.varType == varType) =>
        s"\\forall ${header.parameters.map(_.variable).map(v => colorText(colors(v), v.name)).mkString(",")} \\in ${writeType(varType)}: "
      case parameters =>
        s"\\forall\\left( ${parameters.map(d => s"${colorText(colors(d.variable), d.name)} \\in ${writeType(d.varType)}").mkString(",")}\\right): "
    }

  def writeType(t: Type): String = t match {
    case SimpleType(n) => expWriter.writeType(n)
    case FuncType(from, to) => s"\\left(${writeType(from)} \\rightarrow ${writeType(to)}\\right)"
    case TupleType(ts) => s"\\left(${ts.map(writeType).mkString(",")}\\right)"
  }

  def writeExp(exp: Exp, textColor: PathTree[String] = PathTree.empty, backColor: PathTree[String] = PathTree.empty, binding: Int = 0): String = {
    expWriter(exp, textColor, backColor, binding)
  }

  def colorText(color: Option[String], toColor: String): String = if (color.isEmpty) toColor else colorText(color.get, toColor)
  def colorBack(color: Option[String], toColor: String): String = if (color.isEmpty) toColor else colorBack(color.get, toColor)

  def colorText(color: String, toColor: String): String = if (this.color) s"\\textcolor{$color}{$toColor}" else toColor
  def colorBack(color: String, toColor: String): String = if (this.color) s"\\fcolorbox{$color}{white}{$$$toColor$$}" else toColor
}
