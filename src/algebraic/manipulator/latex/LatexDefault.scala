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
    LatexWriter.operationWriters += (("sum", sum))
    LatexWriter.operationWriters += (("prod", prod))
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
      case Operation(_, List(a, b)) =>
        if (binding > bind)
          s"\\left(${writeExp(a, textColor(0), backColor(0), bind)}$separator${writeExp(b, textColor(1), backColor(1), bind + 1)}\\right)"
        else
          s"${writeExp(a, textColor(0), backColor(0), bind)}$separator${writeExp(b, textColor(1), backColor(1), bind + 1)}"
      case _ => defaultWriter(op, textColor, backColor, binding)
    }

  def div(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, List(a, b)) =>
      s"\\frac{${writeExp(a, textColor(0), backColor(0))}}{${writeExp(b, textColor(1), backColor(1))}}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def pow(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, List(a, b)) =>
      if (binding > 2)
        s"\\left(${writeExp(a, textColor(0), backColor(0), 3)}\\right)^{${writeExp(b, textColor(1), backColor(1))}}"
      else
        s"{${writeExp(a, textColor(0), backColor(0), 3)}}^{${writeExp(b, textColor(1), backColor(1))}}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def sum(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, List(Lambda(List(i), e), a, b)) =>
      s"\\displaystyle\\sum_{${writeExp(i)}=${writeExp(a, textColor(1), backColor(1))}}^{${writeExp(b, textColor(2), backColor(2))}}${writeExp(e, textColor(0)(0), backColor(0)(0), Int.MaxValue)}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def prod(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, List(Lambda(List(i), e), a, b)) =>
      s"\\displaystyle\\prod_{${writeExp(i)}=${writeExp(a, textColor(1), backColor(1))}}^{${writeExp(b, textColor(2), backColor(2))}}${writeExp(e, textColor(0)(0), backColor(0)(0), Int.MaxValue)}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def lim(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, List(v, Lambda(List(d), f))) =>
      s"\\displaystyle\\lim_{${writeExp(d)} \\to ${writeExp(v, textColor(0), backColor(0))}}${writeExp(f, textColor(1)(0), backColor(1)(0), Int.MaxValue)}"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }

  def diff(op: Operation, textColor: PathTree[String], backColor: PathTree[String], binding: Int): String = op match {
    case Operation(_, List(Operation(f, List(x)))) =>
      s"\\frac{\\mathrm d}{\\mathrm d ${writeExp(x, textColor(1), backColor(1), 3)}}${writeExp(f, textColor(0), backColor(0), Int.MaxValue)}"
    case Operation(_, List(f)) =>
      writeExp(f, textColor(0), backColor(0), Int.MaxValue) + "'"
    case _ => defaultWriter(op, textColor, backColor, binding)
  }
}
