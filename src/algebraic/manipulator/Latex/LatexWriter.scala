package algebraic.manipulator.Latex

import java.time.LocalDate
import java.time.format.TextStyle
import java.util.Locale

import algebraic.manipulator._
import algebraic.manipulator.manipulation._

object LatexWriter {
  var operationWriters = Map.empty[String, (Operation, PathTree[String], PathTree[String], Int) => String]
  var typeNames = Map.empty[String, String]
  var colors = List("red", "blue", "olive", "orange", "yellow")

  def apply(project: Project, title: String, author: String): String = {
    def getFiles(sub: Project, list: List[String]): List[WorkFile] = sub match {
      case f: Project.Folder =>
        Graph.topologicalSort[String, Project](f.map, (k, p) => p.dependencies(project, k :: list).filter(_.tail == list).map(_.head))
          .flatMap{case (k, p) => getFiles(p, k :: list)}
      case Project.File(file) => List(file)
    }

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
      getFiles(project, List.empty).map(writeFile(project, _)).mkString("\n") +
      "\n\\end{document}"
  }

  def writeFile(project: Project, file: WorkFile): String = {
    val finder = file.find(project)

    s"\\chapter{${file.path.last}}\n" +
      "Given the following assumptions:\n" +
      file.names.filter(file.get(_).isInstanceOf[Assumption]).map(name => {
        "\\\\\n" +
          s"\\label{${(file.path ++ List(name)).mkString(":")}}\n" +
          writeAssumption(file.get(name).asInstanceOf[Assumption]) +
          "\n"
      }).mkString +
      file.names.filter(!file.get(_).isInstanceOf[Assumption]).map(name => {
        s"\\section{$name}\n" +
          s"\\label{${(file.path ++ List(name)).mkString(":")}}\n" +
          writeIdentity(finder, file.get(name))
      }).mkString
  }

  def writeAssumption(assumption: Assumption): String =
    s"$$${writeDefinition(assumption.header)}: ${assumption.result.map(writeExp(_)).mkString("=")}$$"

  def writeIdentity(finder: Project.Finder, identity: Identity): String = identity match {
    case p: Proof =>
      var exps = List.fill(p.count)(p.origin)
      var res = ""

      var backColor = PathTree.empty[String]
      var textColor = PathTree.empty[String]

      for (manipulation <- p.manipulations) {
        backColor = getInputColors(finder, exps, manipulation)
        res += "$$" + (exps.indices zip exps).map{case (i, exp) => writeExp(exp, textColor(i), backColor(i))}.mkString("=") + "$$\n"

        exps = manipulation(finder, exps)
        textColor = getOutputColors(finder, exps, manipulation)

        res += s"{\\color{gray}${writeManipulation(finder, manipulation)}}\n"
      }

      backColor = PathTree.empty
      res += "$$" + (exps.indices zip exps).map{case (i, exp) => writeExp(exp, textColor(i), backColor(i))}.mkString("=") + "$$\n"

      res
    case p: InductionProof => "Proof by induction\n" +
      s"\\subsection{$$${p.inductives.toList.map{case (v, exp) => s"$v = ${writeExp(exp)}"}.mkString(",")}$$}\n" +
      p.inductives.keySet.map(v => s"\\subsection{$v'=$v+1}\n${writeIdentity(finder, p.up(v))}\n\\subsection{$v'=$v-1}\n${writeIdentity(finder, p.down(v))}")
    case p: AssumedProof =>
      var exps = p.origin
      var res = ""

      var backColor = PathTree.empty[String]
      var textColor = PathTree.empty[String]

      for (manipulation <- p.manipulations) {
        backColor = getInputColors(finder, exps, manipulation)
        res += "$$" + (exps.indices zip exps).map{case (i, exp) => writeExp(exp, textColor(i), backColor(i))}.mkString("=") + "$$\n"

        exps = manipulation(finder, exps)
        textColor = getOutputColors(finder, exps, manipulation)

        res += s"{\\color{gray}${writeManipulation(finder, manipulation)}}\n"
      }

      backColor = PathTree.empty
      res += "$$" + (exps.indices zip exps).map{case (i, exp) => writeExp(exp, textColor(i), backColor(i))}.mkString("=") + "$$\n"

      res
  }

  def getInputColors(finder: Project.Finder, exps: List[Exp], manipulation: Manipulation): PathTree[String] = manipulation match {
    case Call(_, _) => PathTree.empty
    case Substitute(positions, path, from, _, _, _) =>
      val identity = finder(path)
      val parameters = identity.header.parameters.map(_.variable)
      positions :: identity.result(from).tree.filter(parameters.contains).map(parameters.indexOf(_)).map(colors)
    case ToEval(positions, parameters) =>
      positions :: (parameters.indices zip parameters.map(_.positions)).map{case (i, p) => p.map(_ :> colors(i)).getOrElse(PathTree.empty)}.fold(PathTree.empty)(_|_)
    case FromEval(positions) => positions :> colors.head
    case Rename(positions, _, _) => positions :> colors.head
  }

  def getOutputColors(finder: Project.Finder, exps: List[Exp], manipulation: Manipulation): PathTree[String] = manipulation match {
    case Call(_, _) => PathTree.empty
    case Substitute(positions, path, _, to, _, _) =>
      val identity = finder(path)
      val parameters = identity.header.parameters.map(_.variable)
      positions :: identity.result(to).tree.filter(parameters.contains).map(parameters.indexOf(_)).map(colors)
    case ToEval(positions, parameters) =>
      positions ::
        (Tree.edge(0, 0) :: (parameters.indices zip parameters.map(_.positions)).map{case (i, p) => p.map(_ :> colors(i)).getOrElse(PathTree.empty)}.fold(PathTree.empty)(_|_)
          | parameters.indices.map(i => Tree.edge(i+1) :> colors(i)).fold(PathTree.empty)(_|_))
    case FromEval(positions) => positions :> colors.head
    case Rename(positions, _, _) => positions :> colors.head
  }

  def writeManipulation(finder: Project.Finder, manipulation: Manipulation): String = manipulation match {
    case Call(temp, exp) => s"Call $$${writeExp(exp, exp.tree.filter(_ == temp).map(_ => colors.head))}$$"
    case Substitute(_, path, from, to, _, _) =>
      val identity = finder(path)
      writeIdentityReference(finder.toFull(path), identity.header, List(from, to).map(identity.result))
    case ToEval(_, _) => "Convert to function call"
    case FromEval(_) => "Convert from function call"
    case Rename(_, from, to) => s"Renaming $from to $to"
  }

  def writeIdentityReference(path: List[String], header: Header, exps: List[Exp]): String = {
    val parameters = header.parameters.map(_.variable)
    val equation = exps.map(e => writeExp(e, e.tree.filter(parameters.contains).map(v => colors(parameters.indexOf(v)))))
    s"\\hyperref[${path.mkString(":")}]{$$${writeDefinition(header, v => Some(colors(parameters.indexOf(v))))}: ${equation.mkString("=")}$$}"
  }

  def writeDefinition(header: Header, colors: Variable => Option[String] = _ => None): String = header.parameters match {
    case Nil => ""
    case par :: Nil =>
      s"\\forall ${colorText(colors(par.variable), par.name)} \\in ${writeType(par.varType)}"
    case Definition(varType, _) :: tail if tail.forall(_.varType == varType) =>
      s"\\forall ${header.parameters.map(_.variable).map(v => colorText(colors(v), v.name)).mkString(",")} \\in ${writeType(varType)}"
    case parameters =>
      s"\\forall\\left( ${parameters.map(d => s"${colorText(colors(d.variable), d.name)} \\in ${writeType(d.varType)}").mkString(",")}\\right)"
  }

  def writeType(t: Type): String = t match {
    case SimpleType(n) => typeNames.getOrElse(n, n)
    case FuncType(from, to) => s"\\left(${writeType(from)} \\rightarrow ${writeType(to)}\\right)"
    case TupleType(ts) => s"\\left(${ts.map(writeType).mkString(",")}\\right)"
  }

  def writeExp(exp: Exp, textColor: PathTree[String] = PathTree.empty, backColor: PathTree[String] = PathTree.empty, binding: Int = 0): String = {
    if (textColor.isLeaf)
      colorText(textColor.asInstanceOf[PathTree.Leaf[String]].leaf, writeExp(exp, PathTree.empty, backColor, binding))
    else if (backColor.isLeaf)
      colorBack(backColor.asInstanceOf[PathTree.Leaf[String]].leaf, writeExp(exp, textColor, PathTree.empty, binding))
    else {
      if (!exp.isInstanceOf[Operation] && (textColor.nonEmpty || backColor.nonEmpty))
        throw new IllegalArgumentException(s"Trees must be empty $exp $textColor $backColor")

      exp match {
        case op @ Operation(name, _, _) =>
          if (operationWriters.contains(name))
            operationWriters(name)(op, textColor, backColor, binding)
          else
            defaultWriter(op, textColor, backColor, binding)
        case Variable(name) => name
        case Constant(name) => name
        case IntVal(v) => v.toString
      }
    }
  }

  def defaultWriter(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = {
    val dummies = if (op.dummies.nonEmpty) s"\\left<${op.dummies.mkString(",")}\\right>" else ""
    val parameters = (op.parameters.indices zip op.parameters).map { case (i, exp) => writeExp(exp, textColor(i), backColor(i)) }.mkString(",")
    s"${op.name}$dummies\\left($parameters\\right)"
  }

  def colorText(color: Option[String], toColor: String): String = if (color.isEmpty) toColor else colorText(color.get, toColor)
  def colorBack(color: Option[String], toColor: String): String = if (color.isEmpty) toColor else colorBack(color.get, toColor)

  def colorText(color: String, toColor: String) = s"\\textcolor{$color}{$toColor}"
  def colorBack(color: String, toColor: String) = s"\\fcolorbox{$color}{white}{$$$toColor$$}"
}
