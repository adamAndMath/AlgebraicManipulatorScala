package algebraic.manipulator.read

import java.io.File
import java.nio.file.Path

import algebraic.manipulator._
import algebraic.manipulator.manipulation._
import algebraic.manipulator.read.Tokens._

import scala.io.Source

object ProofReader {
  var manipulationReaders: Map[String, Tokens => Read[Manipulation]] = Map.empty +
    ("call" -> (readCall(_: Tokens).map(c=>c:Manipulation))) +
    ("substitute" -> (readSubstitution(_: Tokens).map(c=>c:Manipulation))) +
    ("rename" -> (readRename(_: Tokens).map(c=>c:Manipulation))) +
    ("toeval" -> (readToEval(_: Tokens).map(c=>c:Manipulation))) +
    ("fromeval" -> (readFromEval(_: Tokens).map(c=>c:Manipulation)))

  val identityReaders: Map[String, Tokens => Read[IdentityTemplate]] = Map.empty +
    ("assume" -> (readAssumption(_: Tokens).map(c=>c:IdentityTemplate))) +
    ("work" -> (readProof(_: Tokens).map(c=>c:IdentityTemplate))) + //TODO:Change work keyword to proof
    ("induction" -> (readInduction(_: Tokens).map(c=>c:IdentityTemplate)))

  def readTree(tokens: Tokens): Read[Tree] =
    {
      if (tokens is OPEN_BRAC) readTreeBrac(tokens)
      else tokens.int().map(Tree.edge(_))
    }.and(_.when(COMMA, readTree))
    .map{case (a, b) => b.map(a :: _).getOrElse(a)}

  def readTreeBrac(tokens: Tokens): Read[Tree] =
    tokens.readList(BAR, BRACKETS, readTree).map(c => if (c.isEmpty) Tree.empty else (c.head /: c.tail)(_|_))

  def readExp(tokens: Tokens): Read[Exp] = tokens.token match {
    case INT(_) => readInt(tokens).map(c=>c)
    case BACKSLASH => readConstant(tokens).map(c=>c)
    case STRING(_) =>
      if (tokens.tail.is(OPEN_PAR) || tokens.tail.is(LESS))
        readOperation(tokens).map(c=>c)
      else
        readVariable(tokens).map(c=>c)
  }

  def readOperation(tokens: Tokens): Read[Operation] =
    tokens.string()
      .and(_.whenBlock(LESSGREAT, _.readList(COMMA, readVariable)))
      .and(_.readList(COMMA, PARENTHESES, readExp))
      .map{case ((name, dum), par) => Operation(name, dum.getOrElse(List.empty), par)}

  def readConstant(tokens: Tokens): Read[Constant] = tokens.expect(BACKSLASH).string().map(Constant)

  def readVariable(tokens: Tokens): Read[Variable] = tokens.string().map(Variable)

  def readInt(tokens: Tokens): Read[IntVal] = tokens.int().map(IntVal)

  def readType(tokens: Tokens): Read[Type] =
    readTypeIn(tokens).and(_.when(ARROW, readTypeIn)).map{case (f, s) => s.map(FuncType(f, _)).getOrElse(f)}

  private def readTypeIn(tokens: Tokens): Read[Type] =
    if (tokens is OPEN_PAR) tokens.readList(COMMA, PARENTHESES, readType).map(TupleType) else tokens.string().map(SimpleType)

  def readDefinition(tokens: Tokens): Read[Definition] =
    readType(tokens).and(_.string()).map{case (tp, name) => Definition(tp, name)}

  def readManipulation(tokens: Tokens): Read[Manipulation] = {
    val read = tokens.string()
    manipulationReaders.getOrElse(read.read, throw new IllegalArgumentException)(read.tokens).ignore(SEMI)
  }

  def readCall(tokens: Tokens): Read[Call] =
    readVariable(tokens).and(readExp).map{case (temp, exp) => Call(temp, exp)}

  def readSubstitution(tokens: Tokens): Read[Substitute] =
    tokens.readList(DOT, _.string())
      .and(_.expect(BRACKETS, t => t.int().expect(ARROW).and(t => t.int())))
      .and(_.whenBlock(LESSGREAT, _.readList(COMMA, readVariable)).map(_.getOrElse(List.empty)))
      .and(_.whenBlock(PARENTHESES, _.readList(COMMA, _.option(DASH, readExp))))
      .expect(COLON).and(readTree).map{
      case ((((path,(from,to)),dum),par),pos) => Substitute(pos, path, from, to, dum, par)
    }

  def readRename(tokens: Tokens): Read[Rename] =
    readVariable(tokens).expect(ARROW).and(readVariable).expect(COLON).and(readTree).map{
      case ((from, to), pos) => Rename(pos, from, to)
    }

  def readToEval(tokens: Tokens): Read[ToEval] =
    tokens.readList(COMMA, PARENTHESES,
      readVariable(_).and(_.when(EQUAL, readExp)).and(_.when(COLON, readTreeBrac)).map{case ((n, v), p) => ToEval.Parameter(n, v, p)}
    ).expect(COLON).and(readTree).map{case (par, pos) => ToEval(pos, par)}

  def readFromEval(tokens: Tokens): Read[FromEval] = readTree(tokens.expect(COLON)).map(FromEval)

  def readHeader(tokens: Tokens): Read[Header] =
    tokens.whenBlock(LESSGREAT, _.readList(COMMA, readVariable))
      .and(_.readList(COMMA, PARENTHESES, readDefinition))
      .map{ case (dum, par) => Header(dum.getOrElse(List.empty), par)}

  def readIdentity(tokens: Tokens): Read[(String, IdentityTemplate)] = {
    val Read((typeName, name), tail) = tokens.string().and(_.string())
    identityReaders.getOrElse(typeName, throw new TokenException(tokens, s"$typeName is not a valid identity type"))(tail)
        .map(ide => name -> ide)
  }

  def readAssumption(tokens: Tokens): Read[AssumptionTemplate] =
    readHeader(tokens).and(_.readList(EQUAL, readExp)).ignore(SEMI).map{case (head, res) => AssumptionTemplate(head, res)}

  def readProof(tokens: Tokens): Read[ProofTemplate] =
    readHeader(tokens)
      .and(_.expect(BLOCK, _.expect("let").int().and(readExp).ignore(SEMI).and(_.whileNot(CLOSE_BLOCK, readManipulation))))
      .and(_.expect(STRING("result")).ignore(BLOCK, _.readList(EQUAL, readExp)))
      .map{ case ((head, ((count, origin), ms)), result) => ProofTemplate(head, result, count, origin, ms)}

  def readInduction(tokens: Tokens): Read[InductionProofTemplate] =
    readHeader(tokens)
      .and(_.expect(BLOCK,
        _.expect("base").readList(COMMA, readVariable(_).expect(EQUAL).and(readExp)).and(_.expect(BLOCK, _.expect("let").int().and(readExp).ignore(SEMI).and(_.whileNot(CLOSE_BLOCK, readManipulation))))
          .and(
            _.whileNot(CLOSE_BLOCK, readVariable(_).and(t => Read[Boolean](t.token match {case PLUS => true; case DASH => false}, t.tail)).and(_.expect(BLOCK, _.whileNot(CLOSE_BLOCK, readManipulation))))
              .map(is => (is :\ (Map.empty[Variable, List[Manipulation]], Map.empty[Variable, List[Manipulation]]))((i, o) => if (i._1._2) (o._1 + (i._1._1 -> i._2), o._2) else (o._1, o._2 + (i._1._1 -> i._2))))
          )
      )).expect("result").and(_.ignore(BLOCK, _.readList(EQUAL, readExp)))
      .map{case ((header, ((base, ((count, origin), baseManip)), (up, down))), result) => InductionProofTemplate(header, result, base.toMap, count, origin, baseManip, up, down)}

  def readFile(path: List[String], tokens: Tokens): FileTemplate =
    tokens.whileMatch(STRING("using"), _.readList(DOT, _.string()).ignore(SEMI))
      .and(_.whileNot(EOF, readIdentity))
      .map{case (using, ides) => FileTemplate(path, using.map(p => p.last -> p).toMap, ides)}.read

  def readFile(projectPath: Path, path: List[String]): FileTemplate = {
    val iPath = path.dropRight(1) ++ List(path.last.substring(0, path.last.lastIndexOf('.')))

    try {
      readFile(iPath, Tokens(Source.fromFile((projectPath /: path) (_.resolve(_)).toFile).toList))
    } catch {
      case e: Exception => throw new IllegalStateException(s"Exception occurred while reading ${iPath.mkString(".")}", e)
    }
  }

  def readProject(path: Path): ProjectTemplate = {
    def rec(f: File, list: List[String]): ProjectTemplate = {
      if (f.isDirectory)
        ProjectTemplate.Folder(f.listFiles().map(f => (if (f.isFile) f.getName.substring(0, f.getName.lastIndexOf('.')) else f.getName) -> rec(f, f.getName :: list)).toMap)
      else
        ProjectTemplate.File(readFile(path, list.reverse))
    }

    rec(path.toFile, List.empty)
  }
}
