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
import algebraic.manipulator.properties.{PropImpl, Property}
import algebraic.manipulator.specifiers.Header
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

  def apply(project: Element): String = {
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
      project.filter{ case e: Environment => e.base.isInstanceOf[WorkFile]; case _ => false }.map(e => writeFile(e.asInstanceOf[Environment])).mkString("\n") +
      "\n\\end{document}"
  }

  def getEnvElements(env: Environment, l: List[(Environment, String, Element)] = Nil): List[(Environment, String, Element)] = env match {
    case Environment.Compound(e, k, elm) => getEnvElements(e, (e, k, elm)::l)
    case _ => l
  }

  def writeFile(file: Environment): String = {
    val es = getEnvElements(file)
    val definitions = es.filter(e => getLocation(e._3) == DefinitionLocation)
    val assumptions = es.filter(e => getLocation(e._3) == AssumptionLocation)
    val proofs = es.filter(e => getLocation(e._3) == ProofLocation)

    s"\\chapter{${file.path.last}}\n" +
      (definitions.map(e => writeElementDefinition(e._2, e._3)) ++
      {
        if (assumptions.nonEmpty)
          "Given the following assumptions:" ::
            assumptions.map(e =>
              s"\\label{${(e._1.path :+ e._2).mkString(":")}}\n" +
                writeAssumption(e._3)
            )
        else Nil
      }).mkString("\\\\\n") + "\n" +
      proofs.map(e => {
        s"\\section{${e._2}}\n" +
          s"\\label{${(e._1.path :+ e._2).mkString(":")}}\n" +
          writeElement(e._1, e._3)
      }).mkString
  }

  def getLocation(element: Element): ElementLocation = element match {
    case PropImpl(_, _, ide) => getLocation(ide)
    case _: Assumption => AssumptionLocation
    case _: AssumedFunction => IgnoredLocation
    case SimpleStructure => IgnoredLocation
    case InductiveStructure(_, _, _) => DefinitionLocation
    case _: ObjectElement => DefinitionLocation
    case _: FunctionElement => DefinitionLocation
    case _: Identity => ProofLocation
    case _: Property => DefinitionLocation
    case _ => throw new IllegalArgumentException(s"${element.getClass} doesn't have a location")
  }

  def writeElementDefinition(name: String, element: Element): String = element match {
    case PropImpl(_, _, ide) => writeElementDefinition(name, ide)
    case AssumedObject => s"Let $name be an object"
    case SimpleObject(exp) => s"Let $$${writeExp(Variable(name))} = ${writeExp(exp)}$$"
    case InductiveStructure(header, base, steps) =>
      val typeOut = writeType(SimpleType(name, Nil))
      s"Let $$$typeOut$$ be the smallest set that satisfies " +
        s"$$${writeDefinition(header)}${writeDefinition(Header(Nil, Nil, base.params))}${writeExp(base.exp)} \\in $typeOut$$" +
        steps.map(step => s" and $$${writeDefinition(Header(Nil, Nil, Definition(SimpleType(name, Nil), step.v) :: step.params))}${writeExp(step.exp)} \\in $typeOut$$").mkString
    case SimpleFunction(header, exp) => s"Let $$${writeExp(Operation(Variable(name), header.parameters.map(_.variable)))} = ${writeExp(exp)}$$"
    case InductiveFunction(header, base, steps) =>
      s"Let $$${writeExp(Operation(Variable(name), header.parameters.map(_.variable)))} = \n" +
        "  \\begin{cases}\n" +
        s"    ${base.exp} & \\quad ${base.inductive} = ${base.value}" +
        s"${steps.map(s => s"\\\\\n    ${s.exp} & \\quad ${s.step}").mkString}" +
        "\n  \\end{cases}$"
    case Property(_, _, ideHeader, ide) =>
      s"$$$name\\Leftrightarrow ${writeDefinition(ideHeader)}${ide.map(writeExp(_)).mkString("=")}$$"
  }

  def writeAssumption(element: Element): String = element match {
    case PropImpl(_, _, ide) => writeAssumption(ide)
    case a: Assumption => s"$$${writeDefinition(a.header)}${a.result.map(writeExp(_)).mkString("=")}$$"
  }

  def writeElement(env: Environment, element: Element): String = element match {
    case PropImpl(_, _, ide) => writeElement(env, ide)
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
    case Substitute(positions, path, from, _, _) =>
      env.find(path, _.isInstanceOf[Substitutable]).get match {
        case ide: Identity =>
          val parameters = ide.header.parameters.map(_.variable)
          positions :: ide.result(from).tree.filter(parameters.contains).map(parameters.indexOf(_)).map(colors)
        case Property(_, _, ideHeader, ide) =>
          val parameters = ideHeader.parameters.map(_.variable)
          positions :: ide(from).tree.filter(parameters.contains).map(parameters.indexOf(_)).map(colors)
      }
    case Rename(positions, _, _) => positions :> colors.head
    case Wrap(_, positions) => positions :> colors.head
    case Unwrap(positions) => positions :> colors.head
  }

  def getOutputColors(env: Environment, exps: List[Exp], manipulation: Manipulation): PathTree[String] = manipulation match {
    case Call(_, _) => PathTree.empty
    case Substitute(positions, path, _, to, _) =>
      env.find(path, _.isInstanceOf[Substitutable]).get match {
        case ide: Identity =>
          val parameters = ide.header.parameters.map(_.variable)
          positions :: ide.result(to).tree.filter(parameters.contains).map(parameters.indexOf(_)).map(colors)
        case Property(_, _, ideHeader, ide) =>
          val parameters = ideHeader.parameters.map(_.variable)
          positions :: ide(to).tree.filter(parameters.contains).map(parameters.indexOf(_)).map(colors)
      }
    case Rename(positions, _, _) => positions :> colors.head
    case Wrap(_, positions) => positions :> colors.head
    case Unwrap(positions) => positions :> colors.head
  }

  def writeManipulation(env: Environment, manipulation: Manipulation): String = manipulation match {
    case Call(temp, exp) => s"Call $$${writeExp(exp, exp.tree.filter(_ == temp).map(_ => colors.head))}$$"
    case Substitute(_, path, from, to, _) =>
      env.find(path, _.isInstanceOf[Substitutable]).get match {
        case ide: Identity =>
          writeIdentityReference(env.toFull(path, _.isInstanceOf[Substitutable]).get, ide.header, List(from, to).map(ide.result))
        case Property(_, _, ideHeader, ide) =>
          writeIdentityReference(env.toFull(path, _.isInstanceOf[Substitutable]).get, ideHeader, List(from, to).map(ide))
      }
    case Rename(_, from, to) => s"Renaming $from to $to"
    case Wrap(Variable(name), _) => env.find(List(name), _.isInstanceOf[Wrapable]).get match {
      case SimpleObject(exp) => writeIdentityReference(env.toFull(List(name), _.isInstanceOf[Wrapable]).get, Header(Nil, Nil, Nil), List(Variable(name), exp))
      case SimpleFunction(header, exp) => writeIdentityReference(env.toFull(List(name), _.isInstanceOf[Wrapable]).get, header, List(Operation(Variable(name), header.parameters.map(_.variable)), exp))
    }
    case Wrap(Lambda(params, e), _) =>
      s"Wrapping $$${params.map(writeExp(_)).mkString("(", ", ", ")")} \\rightarrow ${writeExp(e)}$$"
    case Wrap(_, _) => "Wrapping"
    case Unwrap(_) => "Unwrapping"
  }

  def writeIdentityReference(path: List[String], header: Header, exps: List[Exp]): String = {
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
    case SimpleType(n, gen) => expWriter.writeType(n) + s"\\left(${gen.map(writeType).mkString(",")}\\right)"
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
