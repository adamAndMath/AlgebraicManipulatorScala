package algebraic.manipulator.read

import scala.annotation.tailrec

object Tokens {
  def apply(code: List[Char]): Tokens = apply(1, 1, code)

  def apply(pos: Int, line: Int, code: List[Char]): Tokens = {
    @tailrec
    def go(prev: Tokens, pos: Int, line: Int, code: List[Char]): Tokens = {
      if (code.isEmpty)
        prev
      else code.head match {
        case ' ' => go(prev, pos + 1, line, code.tail)
        case '\t' => go(prev, pos + 1, line, code.tail)
        case '\r' => go(prev, pos + 1, line, code.tail)
        case '\n' => go(prev, 1, line + 1, code.tail)
        case '<' => go(Token(pos, line, LESS, prev), pos + 1, line, code.tail)
        case '>' => go(Token(pos, line, GREAT, prev), pos + 1, line, code.tail)
        case '(' => go(Token(pos, line, OPEN_PAR, prev), pos + 1, line, code.tail)
        case ')' => go(Token(pos, line, CLOSE_PAR, prev), pos + 1, line, code.tail)
        case '[' => go(Token(pos, line, OPEN_BRAC, prev), pos + 1, line, code.tail)
        case ']' => go(Token(pos, line, CLOSE_BRAC, prev), pos + 1, line, code.tail)
        case '{' => go(Token(pos, line, OPEN_BLOCK, prev), pos + 1, line, code.tail)
        case '}' => go(Token(pos, line, CLOSE_BLOCK, prev), pos + 1, line, code.tail)
        case '|' => go(Token(pos, line, BAR, prev), pos + 1, line, code.tail)
        case '.' => go(Token(pos, line, DOT, prev), pos + 1, line, code.tail)
        case ',' => go(Token(pos, line, COMMA, prev), pos + 1, line, code.tail)
        case ':' => go(Token(pos, line, COLON, prev), pos + 1, line, code.tail)
        case ';' => go(Token(pos, line, SEMI, prev), pos + 1, line, code.tail)
        case '/' =>
          if (code.tail.head == '/') {
            val skip = code.takeWhile(_ != '\n').length
            go(prev, pos + skip, line, code.drop(skip))
          } else
            go(Token(pos, line, SLASH, prev), pos + 1, line, code.tail)
        case '\\' => go(Token(pos, line, BACKSLASH, prev), pos + 1, line, code.tail)
        case '=' => go(Token(pos, line, EQUAL, prev), pos + 1, line, code.tail)
        case '+' => go(Token(pos, line, PLUS, prev), pos + 1, line, code.tail)
        case '-' =>
          if (code.tail.head == '>')
            go(Token(pos, line, ARROW, prev), pos + 2, line, code.tail.tail)
          else
            go(Token(pos, line, DASH, prev), pos + 1, line, code.tail)
        case c =>
          if (c.isDigit) {
            val num = code.takeWhile(_.isDigit).mkString
            prev match {
              case Token(p, l, STRING(s), pre) if p + s.length == pos && l == line =>
                go(Token(p, l, STRING(s + num), pre), pos + num.length, line, code.drop(num.length))
              case _ => go(Token(pos, line, INT(num.toInt), prev), pos + num.length, line, code.drop(num.length))
            }
          } else {
            prev match {
              case Token(p, l, STRING(s), pre) if p + s.length == pos && l == line =>
                go(Token(p, l, STRING(s + c), pre), pos + 1, line, code.tail)
              case _ => go(Token(pos, line, STRING(c.toString), prev), pos + 1, line, code.tail)
            }
          }
      }
    }

    @tailrec
    def list(token: Tokens, l: List[(Int, Int, ProofToken)]): List[(Int, Int, ProofToken)] = token match {
      case Token(p, ln, t, pre) => list(pre, (p, ln, t) :: l)
      case EndToken => l
    }

    val l = list(go(EndToken, 1, 1, code), List.empty)
    (l :\ (EndToken:Tokens))((p, next) => Token(p._1, p._2, p._3, next))
  }

  case class Read[T](read: T, tokens: Tokens) {
    def expect(t: ProofToken): Read[T] = Read(read, tokens.expect(t))
    def ignore(t: ProofToken): Read[T] = Read(read, tokens.ignore(t))
    def expect(str: String): Read[T] = expect(STRING(str))
    def ignore(str: String): Read[T] = ignore(STRING(str))
    def map[U](f: T=>U): Read[U] = Read(f(read), tokens)
    def and[U](reader: Tokens => Read[U]): Read[(T,U)] = {
      val r = reader(tokens)
      Read((read, r.read), r.tokens)
    }
  }

  sealed trait Tokens {
    def token: ProofToken
    def tail: Tokens

    def position: String

    def is(token: ProofToken): Boolean = this.token == token
    def expect(t: ProofToken): Tokens = if (token == t) tail else throw new UnexpectedTokenException(this, t)
    def expect(str: String): Tokens = expect(STRING(str))
    def expect[T](par: Block, reader: Tokens => Read[T]): Read[T] = reader(expect(par.open)).expect(par.close)

    def ignore(t: ProofToken): Tokens = if (token == t) tail else this
    def ignore[T](par: Block, reader: Tokens => Read[T]): Read[T] = if (token == par.open) reader(tail).expect(par.close) else reader(this)

    def string(): Read[String] = token match {
      case STRING(str) => Read(str, tail)
      case _ => throw new UnexpectedTokenException(this, STRING("abc"))
    }

    def int(): Read[Int] = token match {
      case INT(i) => Read(i, tail)
      case _ => throw new UnexpectedTokenException(this, INT(123))
    }

    def readList[T](separator: ProofToken, reader: Tokens => Read[T]): Read[List[T]] =
      reader(this).and(_.whileMatch(separator, reader)).map{ case (e, l) => e :: l}

    def readList[T](separator: ProofToken, block: Block, reader: Tokens => Read[T]): Read[List[T]] = {
      val t = expect(block.open)
      if (t is block.close)
        Read(List.empty, t.tail)
      else
        t.readList(separator, reader).expect(block.close)
    }

    def whileMatch[T](cond: ProofToken, reader: Tokens => Read[T]): Read[List[T]] =
      if (is(cond)) reader(tail).and(_.whileMatch(cond, reader)).map{ case (e, l) => e :: l}
      else Read(List.empty, this)

    def whileNot[T](end: ProofToken, reader: Tokens => Read[T]): Read[List[T]] =
      if (is(end)) Read(List.empty, this)
      else reader(this).and(_.whileNot(end, reader)).map{ case (e, l) => e :: l}

    def when[T](cond: ProofToken, reader: Tokens => Read[T]): Read[Option[T]] =
      if (is(cond)) reader(tail).map(Some(_))
      else Read(None, this)

    def whenBlock[T](cond: Block, reader: Tokens => Read[T]): Read[Option[T]] =
      if (is(cond.open)) reader(tail).expect(cond.close).map(Some(_))
      else Read(None, this)

    def option[T](none: ProofToken, reader: Tokens => Read[T]): Read[Option[T]] =
      if (is(none)) Read(None, tail)
      else reader(this).map(Some(_))
  }

  case object EndToken extends Tokens {
    override def token: ProofToken = EOF
    override def tail: Tokens = throw new TokenException(this, "End of file")
    override def position: String = s"EOF"
  }

  case class Token(pos: Int, line: Int, token: ProofToken, tail: Tokens) extends Tokens {
    override def position: String = s"$line, $pos"
  }

  sealed trait ProofToken

  case object EOF extends ProofToken
  case object LESS extends ProofToken
  case object GREAT extends ProofToken
  case object OPEN_PAR extends ProofToken
  case object CLOSE_PAR extends ProofToken
  case object OPEN_BRAC extends ProofToken
  case object CLOSE_BRAC extends ProofToken
  case object OPEN_BLOCK extends ProofToken
  case object CLOSE_BLOCK extends ProofToken
  case object BAR extends ProofToken
  case object DOT extends ProofToken
  case object COMMA extends ProofToken
  case object COLON extends ProofToken
  case object SEMI extends ProofToken
  case object SLASH extends ProofToken
  case object BACKSLASH extends ProofToken
  case object EQUAL extends ProofToken
  case object PLUS extends ProofToken
  case object DASH extends ProofToken
  case object ARROW extends ProofToken
  case class STRING(str: String) extends ProofToken
  case class INT(i: Int) extends ProofToken

  sealed abstract class Block(val open: ProofToken, val close: ProofToken)

  case object PARENTHESES extends Block(OPEN_PAR, CLOSE_PAR)
  case object BRACKETS extends Block(OPEN_BRAC, CLOSE_BRAC)
  case object BLOCK extends Block(OPEN_BLOCK, CLOSE_BLOCK)
  case object LESSGREAT extends Block(LESS, GREAT)

  class TokenException(token: Tokens, msg: String) extends RuntimeException {
    override def getMessage: String = s"${token.position}: $msg"
  }

  class UnexpectedTokenException(token: Tokens, expected: ProofToken) extends RuntimeException {
    override def getMessage: String = s"Unexpected token at ${token.position}: expected $expected, but received ${token.token}"
  }
}
