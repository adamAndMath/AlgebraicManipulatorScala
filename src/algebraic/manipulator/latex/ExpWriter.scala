package algebraic.manipulator.latex

import algebraic.manipulator._
import algebraic.manipulator.latex.ExpWriter.ExpCase
import algebraic.manipulator.read.{ProofReader, Tokens}
import algebraic.manipulator.read.Tokens._

import scala.io.Source

case class ExpWriter(typeNames: Map[String, String], varNames: Map[Variable, String], expCases: List[ExpCase]) {
  def ++(expWriter: ExpWriter): ExpWriter =
    ExpWriter(expWriter.typeNames ++ typeNames, expWriter.varNames ++ varNames, expCases ++ expWriter.expCases)

  def writeType(name: String): String = typeNames.getOrElse(name, name)

  def apply(exp: Exp, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = {
    if (textColor.isLeaf)
      return LatexWriter.colorText(textColor.asInstanceOf[PathTree.Leaf[String]].leaf, apply(exp, PathTree.empty, backColor, binding))
    if (backColor.isLeaf)
      return LatexWriter.colorBack(backColor.asInstanceOf[PathTree.Leaf[String]].leaf, apply(exp, textColor, PathTree.empty, binding))
    val expCase = expCases.find(_.matches(exp))
    if (expCase.isDefined)
      expCase.get.apply(this, exp, textColor, backColor, binding)
    else exp match {
      case Variable(name) => varNames.getOrElse(Variable(name), name)
      case IntVal(i) => i.toString
      case Operation(f, args) =>
        s"${apply(f, textColor(-1), backColor(-1), Int.MaxValue)}\\left(${
          (args.indices zip args).map{case(i,p) => apply(p, textColor(i), backColor(i), 0)}.mkString(", ")
        }\\right)"
      case Lambda(params, e) =>
        if (binding > 0)
          s"\\left(${params.map(apply(_, PathTree.empty, PathTree.empty, 0)).mkString(", ")} \\rightarrow ${apply(e, textColor(0), backColor(0), 0)}\\right)"
        else
          s"${params.map(apply(_, PathTree.empty, PathTree.empty, 0)).mkString(", ")} \\rightarrow ${apply(e, textColor(0), backColor(0), 0)}"
    }
  }
}

object ExpWriter {
  case class ExpCase(variables: List[Variable], exp: Exp, layout: ExpLayout) {
    def apply(w: ExpWriter, e: Exp, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = {
      val (bound, params) = exp.matchExp(e, variables.map(_ -> None).toMap, variables.map(_ -> None).toMap).get
      layout(w, e, textColor, backColor, binding, exp, params.map{case (k, v) => k -> v.orElse(bound(k))})
    }

    def matches(e: Exp): Boolean =
      try {
        exp.matchExp(e, variables.map(_ -> None).toMap, variables.map(_ -> None).toMap).exists{case (bound, params) => bound.forall{case (k, b) => b.flatMap(b => params(k).map(b == _)).getOrElse(true)}}
      } catch { case _: Throwable => false}
  }

  sealed trait ExpLayout {
    def apply(w: ExpWriter, e: Exp, textColor: PathTree[String], backColor: PathTree[String], binding: Int, pattern: Exp, params: Map[Variable, Option[Exp]]): String = this match {
      case LayoutConcat(parts) => parts.map(_(w, e, textColor, backColor, binding, pattern, params)).mkString
      case LayoutString(str) => str
      case LayoutParentheses(l, b) =>
        val str = l(w, e, textColor, backColor, binding, pattern, params)
        if (binding > b) s"\\left($str\\right)" else str
      case LayoutParameter(p, b) =>
        if (pattern.getBound.contains(p)) {
          w(params(p).getOrElse(throw new IllegalStateException(s"Could not infer $p")), PathTree.empty, PathTree.empty, 0)
        } else {
          val paths = pattern.tree.filter(_ == p).dropVal.toPaths
          if (paths.length != 1) throw new IllegalStateException(s"Could not infer $p")
          w(params(p).get, (textColor /: paths.head)(_(_)), (backColor /: paths.head)(_(_)), b)
        }
      case LayoutPath(path, b) =>
        w(e.get(Tree.from(path)).find(_ => true).get, (textColor /: path)(_(_)), (backColor /: path)(_(_)), b)
    }
  }

  case class LayoutString(str: String) extends ExpLayout
  case class LayoutParentheses(layout: ExpLayout, binding: Int) extends ExpLayout
  case class LayoutParameter(par: Variable, binding: Int) extends ExpLayout
  case class LayoutPath(path: List[Int], binding: Int) extends ExpLayout
  case class LayoutConcat(parts: List[ExpLayout]) extends ExpLayout

  def apply(path: java.nio.file.Path): ExpWriter = ExpWriter(Tokens(Source.fromFile(path.toFile).toList))

  def apply(tokens: Tokens): ExpWriter = {
    var stream = tokens
    var typeNames: Map[String, String] = Map.empty
    var varNames: Map[Variable, String] = Map.empty
    var cases: List[ExpCase] = Nil

    while (!(stream is EOF)) {
      val (key, tail) = stream.string()
      stream = tail

      key match {
        case "type" =>
          val (typeName, tail) = readTypeName(stream)
          typeNames += typeName
          stream = tail
        case "var" =>
          val (varName, tail) = readVarName(stream)
          varNames += varName
          stream = tail
        case "exp" =>
          val (expCase, tail) = readCase(stream)
          cases :+= expCase
          stream = tail
      }
    }

    ExpWriter(typeNames, varNames, cases)
  }

  def readTypeName(tokens: Tokens): Read[(String, String)] = {
    val (t, t1) = tokens.string()
    val (s, t2) = t1.expect(IMPLY).sstring()

    (t -> s, t2)
  }

  def readVarName(tokens: Tokens): Read[(Variable, String)] = {
    val (v, t1) = ProofReader.readVariable(tokens)
    val (s, t2) = t1.expect(IMPLY).sstring()

    (v -> s, t2)
  }

  def readCase(tokens: Tokens): Read[ExpCase] = {
    val (vars, t1) = tokens.expect(PARENTHESES, _.readList(COMMA, ProofReader.readVariable))
    val (exp, t2) = ProofReader.readExp(t1)
    val (layout, t3) = readLayout(t2.expect(IMPLY))
    (ExpCase(vars, exp, layout), t3)
  }

  def readLayout(tokens: Tokens): Read[ExpLayout] = {
    val (ls, tail) = tokens.readList(PLUS, tokens => {
      tokens.token match {
        case SSTRING(s) => (LayoutString(s), tokens.tail)
        case STRING(_) =>
          val (v, t1) = ProofReader.readVariable(tokens)
          val (b, t2) = readBinding(t1)
          (LayoutParameter(v, b), t2)
        case OPEN_BRAC =>
          val (p, t1) = tokens.expect(BRACKETS, _.readList(COMMA, _.int()))
          val (b, t2) = readBinding(t1)
          (LayoutPath(p, b), t2)
        case OPEN_PAR =>
          val (l, t1) = tokens.expect(PARENTHESES, readLayout)
          val (b, t2) = readBinding(t1)
          (LayoutParentheses(l, b), t2)
      }
    })

    (if (ls.length == 1) ls.head else LayoutConcat(ls), tail)
  }

  def readBinding(tokens: Tokens): Read[Int] = {
    val (b, tail) = tokens.when(COLON, t => if (t is "inf") (Int.MaxValue, t.tail) else t.int())
    (b.getOrElse(0), tail)
  }
}