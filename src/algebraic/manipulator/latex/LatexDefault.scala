package algebraic.manipulator.latex

import algebraic.manipulator.latex.LatexWriter.{defaultWriter, writeExp}
import algebraic.manipulator._

object LatexDefault {
  def setup(): Unit = {
    LatexWriter.operationWriters += "add" -> getBiWriter(" + ", 1)
    LatexWriter.operationWriters += "sub" -> getBiWriter(" - ", 1)
    LatexWriter.operationWriters += "mult" -> getBiWriter(" \\cdot ", 2)
    LatexWriter.operationWriters += (("div", div))
    LatexWriter.operationWriters += (("pow", pow))
    LatexWriter.operationWriters += (("rec", rec))
    LatexWriter.operationWriters += (("sum", sum))
    LatexWriter.operationWriters += (("prod", prod))
    LatexWriter.operationWriters += (("eval", eval))
    LatexWriter.operationWriters += (("func", func))
    LatexWriter.operationWriters += (("lim", lim))
    LatexWriter.operationWriters += (("diff", diff))

    LatexWriter.typeNames += "Natural" -> "\\mathbb{N}"
    LatexWriter.typeNames += "Integer" -> "\\mathbb{Z}"
    LatexWriter.typeNames += "Rational" -> "\\mathbb{Q}"
    LatexWriter.typeNames += "Real" -> "\\mathbb{R}"
    LatexWriter.typeNames += "Complex" -> "\\mathbb{C}"
  }

  def getBiWriter(separator: String, bind: Int): (Operation, PathTree[String], PathTree[String], Int) => String =
    (op, textColor, backColor, binding) => op match {
      case Operation(_, Nil, List(a, b)) =>
        if (binding > bind)
          s"\\left(${writeExp(a, textColor(0), backColor(0), bind)}$separator${writeExp(b, textColor(1), backColor(1), bind + 1)}\\right)"
        else
          s"${writeExp(a, textColor(0), backColor(0), bind)}$separator${writeExp(b, textColor(1), backColor(1), bind + 1)}"
      case _ => defaultWriter(op, textColor, backColor, binding)
    }

  def div(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, Nil, List(a, b)) =>
      s"\\frac{${writeExp(a, textColor(0), backColor(0))}}{${writeExp(b, textColor(1), backColor(1))}}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def pow(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, Nil, List(a, b)) =>
      if (binding > 2)
        s"\\left(${writeExp(a, textColor(0), backColor(0), 3)}\\right)^{${writeExp(b, textColor(1), backColor(1))}}"
      else
        s"{${writeExp(a, textColor(0), backColor(0), 3)}}^{${writeExp(b, textColor(1), backColor(1))}}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def rec(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, List(d, r), List(e, i, n)) =>
      s"\\underset{${writeExp(d)}=1..${writeExp(n, textColor(2), backColor(2))}}{\\overset{${writeExp(r)}=${writeExp(i, textColor(1), backColor(1))}}{\\mathrm{R}}}${writeExp(e, textColor(0), backColor(0), Int.MaxValue)}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def sum(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, List(i), List(e, a, b)) =>
      s"\\displaystyle\\sum_{${writeExp(i)}=${writeExp(a, textColor(1), backColor(1))}}^{${writeExp(b, textColor(2), backColor(2))}}${writeExp(e, textColor(0), backColor(0), Int.MaxValue)}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def prod(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, List(i), List(e, a, b)) =>
      s"\\displaystyle\\prod_{${writeExp(i)}=${writeExp(a, textColor(1), backColor(1))}}^{${writeExp(b, textColor(2), backColor(2))}}${writeExp(e, textColor(0), backColor(0), Int.MaxValue)}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def eval(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, Nil, (v: Variable) :: tail) =>
      s"${writeExp(v, textColor(0), backColor(0))}\\left(${(tail.indices zip tail).map{case (i,e) => writeExp(e, textColor(i+1), backColor(i+1))}.mkString(", ")}\\right)"
    case Operation(_, Nil, f :: tail) =>
      s"${writeExp(f, textColor(0), backColor(0), Int.MaxValue)}\\circ\\left(${(tail.indices zip tail).map{case (i,e) => writeExp(e, textColor(i+1), backColor(i+1))}.mkString(", ")}\\right)"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def func(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String =
    s"\\underset{${op.dummies.map(writeExp(_)).mkString(",")}}{func}\\left(${(op.parameters.indices zip op.parameters).map{case (i, e) => writeExp(e, textColor(i), backColor(i))}.mkString(",")}\\right)"

  def lim(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, List(d), List(v, f)) =>
      s"\\displaystyle\\lim_{${writeExp(d)} \\to ${writeExp(v, textColor(0), backColor(0))}}${writeExp(f, textColor(1), backColor(1), Int.MaxValue)}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def diff(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, Nil, List(f, x)) =>
      s"\\frac{\\mathrm d}{\\mathrm d ${writeExp(x, textColor(1), backColor(2), 3)}}${writeExp(f, textColor(0), backColor(0), Int.MaxValue)}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }
}
