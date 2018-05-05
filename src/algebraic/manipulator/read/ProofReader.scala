package algebraic.manipulator.read

import java.io.File
import java.nio.file

import algebraic.manipulator._
import algebraic.manipulator.manipulation._
import algebraic.manipulator.read.Tokens._
import algebraic.manipulator.specifiers.{Header, Specifier, TypeHeader}

import scala.io.Source

object ProofReader {
  var manipulationReaders: Map[String, Tokens => Read[Manipulation]] = Map.empty +
    ("call" -> (readCall(_: Tokens))) +
    ("substitute" -> (readSubstitution(_: Tokens))) +
    ("rename" -> (readRename(_: Tokens))) +
    ("wrap" -> (readWrap(_: Tokens))) +
    ("unwrap" -> (readUnwrap(_: Tokens)))

  var elementReaders: Map[String, Map[String, Tokens => Read[ElementTemplate]]] = Map.empty +
    ("ide" -> IdentityTemplate.readers) +
    ("struct" -> StructureTemplate.readers) +
    ("object" -> ObjectTemplate.readers) +
    ("fn" -> FunctionTemplate.readers) +
    ("prop" -> PropertyTemplate.readers)

  def readTree(tokens: Tokens): Read[Tree] = {
    val (tree, t1) = {
      if (tokens is OPEN_BRAC) readTreeBrac(tokens)
      else {
        val (i, tail) = tokens.int()
        (Tree.edge(i), tail)
      }
    }

    if (t1 is COMMA) {
      val (next, tail) = readTree(t1.tail)
      (tree :: next, tail)
    } else {
      (tree, t1)
    }
  }

  def readTreeBrac(tokens: Tokens): Read[Tree] = {
    val (trees, tail) = tokens.readList(BAR, BRACKETS, readTree)
    if (trees.isEmpty) (Tree.empty, tail)
    else ((trees.head /: trees.tail)(_|_), tail)
  }

  def readExp(tokens: Tokens): Read[Exp] = tokens.token match {
    case INT(_) => readInt(tokens)
    case STRING(_) =>
      val (v, t1) = readVariable(tokens)
      if (t1 is ARROW) {
        val (e, t2) = readExp(t1.tail)
        (Lambda(List(v), e), t2)
      } else if (t1 is OPEN_PAR) {
        val (calls, t2) = t1.whileMatch(OPEN_PAR, _.expect(PARENTHESES, _.readList(COMMA, readExp)))
        (((v: Exp) /: calls)(Operation), t2)
      } else  (v, t1)

    case OPEN_PAR =>
      val (exp, t1) = readExp(tokens.tail)

      if (t1 is COMMA) {
        val (pars, t2) = tokens.expect(PARENTHESES, _.readList(COMMA, readVariable))
        val (e, t3) = readExp(t2.expect(ARROW))
        (Lambda(pars, e), t3)
      } else if (t1 is CLOSE_PAR) {
        val (calls, t2) = t1.tail.whileMatch(OPEN_PAR, _.expect(PARENTHESES, _.readList(COMMA, readExp)))
        ((exp /: calls)(Operation), t2)
      } else (exp, t1)

    case _ => throw new UnexpectedTokenException(tokens, STRING("abc"))
  }

  def readOperation(tokens: Tokens): Read[Operation] = {
    val (name, t1) = tokens.string()
    val (par, t2) = t1.readList(COMMA, PARENTHESES, readExp)

    (Operation(Variable(name), par), t2)
  }

  def readVariable(tokens: Tokens): Read[Variable] = {
    val (name, tail) = tokens.string()
    (Variable(name), tail)
  }

  def readInt(tokens: Tokens): Read[IntVal] = {
    val (i, tail) = tokens.int()
    (IntVal(i), tail)
  }

  def readType(tokens: Tokens): Read[Type] = {
    val (tp, t1) = readTypeIn(tokens)

    if (t1 is ARROW) {
      val (tp2, t2) = readTypeIn(t1.tail)
      (FuncType(tp, tp2), t2)
    } else (tp, t1)
  }

  private def readTypeIn(tokens: Tokens): Read[Type] =
    if (tokens is OPEN_PAR) {
      val (tps, tail) = tokens.readList(COMMA, PARENTHESES, readType)
      (TupleType(tps), tail)
    } else {
      val (tp, t1) = tokens.string()
      val (gen, t2) = t1.whenBlock(BRACKETS, _.readList(COMMA, readType))
      (SimpleType(tp, gen.getOrElse(Nil)), t2)
    }

  def readDefinition(tokens: Tokens): Read[Definition] = {
    val (v, t1) = readVariable(tokens)
    val (tp, t2) = readType(t1.expect(COLON))
    (Definition(tp, v), t2)
  }

  def readManipulation(tokens: Tokens): Read[Manipulation] = {
    val (reader, t1) = tokens.string()
    val (manipulation, t2) = manipulationReaders.getOrElse(reader, throw new TokenException(tokens, s"Undefined manipulation $reader"))(t1)
    (manipulation, t2.ignore(SEMI))
  }

  def readCall(tokens: Tokens): Read[Call] = {
    val (temp, t1) = readVariable(tokens)
    val (exp, t2) = readExp(t1)

    (Call(temp, exp), t2)
  }

  def readSubstitution(tokens: Tokens): Read[Substitute] = {
    val (path, (from, to), t1) = if (tokens is Tokens.TILDE) {
      val (path, t1) = tokens.tail.readList(DOT, _.string())
      (path, 1->0, t1)
    } else {
      val (path, t1) = tokens.readList(DOT, _.string())
      val (ft, t2) = t1.whenBlock(BRACKETS, t => {
        val (from, t1) = t.int()
        val (to, t2) = t1.expect(ARROW).int()
        ((from, to), t2)
      })

      (path, ft.getOrElse(0->1), t2)
    }
    val (spe, t2) = t1.whileNot(COLON, readSpecifier)
    val (pos, t3) = readTree(t2.expect(COLON))

    (Substitute(pos, path, from, to, spe), t3)
  }

  def readRename(tokens: Tokens): Read[Rename] = {
    val (from, t1) = readVariable(tokens)
    val (to, t2) = readVariable(t1.expect(ARROW))
    val (pos, t3) = readTree(t2.expect(COLON))

    (Rename(pos, from, to), t3)
  }

  def readWrap(tokens: Tokens.Tokens): Read[Wrap] = {
    val (exp, t1) = readExp(tokens)
    val (pos, t2) = readTree(t1.expect(COLON))

    (Wrap(exp, pos), t2)
  }

  def readUnwrap(tokens: Tokens.Tokens): Read[Unwrap] = {
    val (pos, tail) = readTree(tokens.expect(COLON))
    (Unwrap(pos), tail)
  }

  def readSpecifier(tokens: Tokens): Read[Specifier] = {
    val (gen, t1) = tokens.whenBlock(BRACKETS, _.readList(COMMA, _.option(DASH, readVariable)))
    val (dum, t2) = t1.whenBlock(LESSGREAT, _.readList(COMMA, _.option(DASH, readVariable)))
    val (par, t3) = t2.whenBlock(PARENTHESES, _.readList(COMMA, _.option(DASH, readExp)))

    (Specifier(gen, dum, par), t3)
  }

  def readTypeHeader(tokens: Tokens): Read[TypeHeader] = {
    val (gen, t1) = tokens.whenBlock(BRACKETS, _.readList(COMMA, readVariable))
    val (par, t2) = t1.readList(COMMA, PARENTHESES, readType)

    (TypeHeader(gen.getOrElse(Nil), par), t2)
  }

  def readHeader(tokens: Tokens): Read[Header] = {
    val (gen, t1) = tokens.whenBlock(BRACKETS, _.readList(COMMA, readVariable))
    val (dum, t2) = t1.whenBlock(LESSGREAT, _.readList(COMMA, readVariable))
    val (par, t3) = t2.whenBlock(PARENTHESES, _.readList(COMMA, readDefinition))

    (Header(gen.getOrElse(Nil), dum.getOrElse(Nil), par.getOrElse(Nil)), t3)
  }

  def readElement(tokens: Tokens): Read[(String, ElementTemplate)] = {
    val (typeName, t1) = tokens.string()
    val (t, t2) = t1.string()
    val (name, t3) = t2.string()

    if (!elementReaders(t).contains(typeName))
      throw new TokenException(tokens, s"$typeName is not a valid $t type")

    val (elm, t4) = elementReaders(t)(typeName)(t3)

    ((name, elm), t4)
  }

  def readUsingAndImport(tokens: Tokens): Read[(Map[String, List[String]], Set[List[String]])] = {
    def r(tokens: Tokens, using: Map[String, List[String]], imports: Set[List[String]]): Read[(Map[String, List[String]], Set[List[String]])] =
      if (tokens is "using") {
        val (p, tail) = tokens.tail.readList(DOT, _.string())
        val path = p
        r(tail.ignore(SEMI), using + (path.last -> path), imports)
      } else if (tokens is "import") {
        val (p, tail) = tokens.tail.readList(DOT, _.string())
        val path = p
        r(tail.ignore(SEMI), using, imports + path)
      } else ((using, imports), tokens)
    r(tokens, Map.empty, Set.empty)
  }

  def readFile(path: List[String], tokens: Tokens): FileTemplate = {
    val ((using, imports), t1) = readUsingAndImport(tokens)
    val (es, _) = t1.whileNot(EOF, readElement)
    FileTemplate(path, using, imports, es)
  }

  def readFile(projectPath: file.Path, path: List[String]): FileTemplate = {
    val iPath = path.dropRight(1) ++ List(path.last.substring(0, path.last.lastIndexOf('.')))

    try {
      readFile(iPath, Tokens(Source.fromFile((projectPath /: path) (_.resolve(_)).toFile).toList))
    } catch {
      case e: Exception => throw new IllegalStateException(s"Exception occurred while reading ${iPath.mkString(".")}", e)
    }
  }

  def readProject(path: file.Path): ProjectTemplate = {
    def rec(f: File, list: List[String]): ProjectTemplate = {
      if (f.isDirectory)
        ProjectTemplate.Folder(f.listFiles().map(f => (if (f.isFile) f.getName.substring(0, f.getName.lastIndexOf('.')) else f.getName) -> rec(f, f.getName :: list)).toMap)
      else
        ProjectTemplate.File(readFile(path, list.reverse))
    }

    rec(path.toFile, List.empty)
  }
}
