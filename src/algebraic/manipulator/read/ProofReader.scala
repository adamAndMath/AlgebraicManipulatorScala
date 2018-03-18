package algebraic.manipulator.read

import java.io.File
import java.nio.file

import algebraic.manipulator._
import algebraic.manipulator.manipulation._
import algebraic.manipulator.read.Tokens._

import scala.io.Source

object ProofReader {
  var manipulationReaders: Map[String, Tokens => Read[Manipulation]] = Map.empty +
    ("call" -> (readCall(_: Tokens))) +
    ("substitute" -> (readSubstitution(_: Tokens))) +
    ("rename" -> (readRename(_: Tokens))) +
    ("toeval" -> (readToEval(_: Tokens))) +
    ("fromeval" -> (readFromEval(_: Tokens)))

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
    case BACKSLASH => readConstant(tokens)
    case STRING(_) =>
      if (tokens.tail.is(OPEN_PAR) || tokens.tail.is(LESS))
        readOperation(tokens)
      else
        readVariable(tokens)
  }

  def readOperation(tokens: Tokens): Read[Operation] = {
    val (name, t1) = tokens.string()
    val (dum, t2) = t1.whenBlock(LESSGREAT, _.readList(COMMA, readVariable))
    val (par, t3) = t2.readList(COMMA, PARENTHESES, readExp)

    (Operation(name, dum.getOrElse(List.empty), par), t3)
  }

  def readConstant(tokens: Tokens): Read[Constant] = {
    val (name, tail) = tokens.expect(BACKSLASH).string()
    (Constant(name), tail)
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
      val (tp, tail) = tokens.string()
      (SimpleType(tp), tail)
    }

  def readDefinition(tokens: Tokens): Read[Definition] = {
    val (tp, t1) = readType(tokens)
    val (name, t2) = t1.string()
    (Definition(tp, name), t2)
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
    val (path, t1) = tokens.readList(DOT, _.string())
    val ((from, to), t2) = t1.expect(BRACKETS, t => {
      val (from, t1) = t.int()
      val (to, t2) = t1.expect(ARROW).int()
      ((from, to), t2)
    })
    val (dum, t3) = t2.whenBlock(LESSGREAT, _.readList(COMMA, readVariable))
    val (par, t4) = t3.whenBlock(PARENTHESES, _.readList(COMMA, _.option(DASH, readExp)))
    val (pos, t5) = readTree(t4.expect(COLON))

    (Substitute(pos, Path(path), from, to, dum.getOrElse(List.empty), par), t5)
  }

  def readRename(tokens: Tokens): Read[Rename] = {
    val (from, t1) = readVariable(tokens)
    val (to, t2) = readVariable(t1.expect(ARROW))
    val (pos, t3) = readTree(t2.expect(COLON))

    (Rename(pos, from, to), t3)
  }

  def readToEval(tokens: Tokens): Read[ToEval] = {
    val (params, t1) = tokens.readList(COMMA, PARENTHESES, tokens => {
      val (v, t1) = readVariable(tokens)
      val (e, t2) = t1.when(EQUAL, readExp)
      val (p, t3) = t2.when(COLON, readTreeBrac)
      (ToEval.Parameter(v, e, p), t3)
    })
    val (pos, t2) = readTree(t1.expect(COLON))

    (ToEval(pos, params), t2)
  }

  def readFromEval(tokens: Tokens): Read[FromEval] = {
    val (pos, tail) = readTree(tokens.expect(COLON))
    (FromEval(pos), tail)
  }

  def readHeader(tokens: Tokens): Read[Header] = {
    val (dum, t1) = tokens.whenBlock(LESSGREAT, _.readList(COMMA, readVariable))
    val (par, t2) = t1.readList(COMMA, PARENTHESES, readDefinition)

    (Header(dum.getOrElse(List.empty), par), t2)
  }

  def readElement(tokens: Tokens): Read[(String, ElementTemplate)] = {
    if (tokens.tail is "struct") {
      val (typeName, t1) = tokens.string()
      val (name, t2) = t1.tail.string()

      if (!StructureTemplate.readers.contains(typeName))
        throw new TokenException(tokens, s"$typeName is not a valid structure type")

      val (structure, t3) = StructureTemplate.readers(typeName)(t2)

      ((name, structure), t3)
    } else {
      val (typeName, t1) = tokens.string()
      val (name, t2) = t1.string()

      if (!IdentityTemplate.readers.contains(typeName))
        throw new TokenException(tokens, s"$typeName is not a valid identity type")

      val (identity, t3) = IdentityTemplate.readers(typeName)(t2)

      ((name, identity), t3)
    }
  }

  def readUsingAndImport(tokens: Tokens): Read[(Map[String, Path], Set[Path])] = {
    def r(tokens: Tokens, using: Map[String, Path], imports: Set[Path]): Read[(Map[String, Path], Set[Path])] =
      if (tokens is "using") {
        val (p, tail) = tokens.tail.readList(DOT, _.string())
        val path = Path(p)
        r(tail.ignore(SEMI), using + (p.last -> path), imports)
      } else if (tokens is "import") {
        val (p, tail) = tokens.tail.readList(DOT, _.string())
        r(tail.ignore(SEMI), using, imports + Path(p))
      } else ((using, imports), tokens)
    r(tokens, Map.empty, Set.empty)
  }

  def readFile(path: List[String], tokens: Tokens): FileTemplate = {
    val ((using, imports), t1) = readUsingAndImport(tokens)
    val (ids, _) = t1.whileNot(EOF, readElement)
    FileTemplate(Path(path), using, imports, ids)
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
