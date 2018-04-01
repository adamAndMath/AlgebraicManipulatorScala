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
        case '~' => go(Token(pos, line, TILDE, prev), pos + 1, line, code.tail)
        case '/' =>
          if (code.tail.head == '/') {
            val skip = code.takeWhile(_ != '\n').length
            go(prev, pos + skip, line, code.drop(skip))
          } else if (code.tail.head == '*') {
            val (p, l, c) = comment(pos+2, line, code.drop(2))
            go(prev, p, l, c)
          } else
            go(Token(pos, line, SLASH, prev), pos + 1, line, code.tail)
        case '\\' => go(Token(pos, line, BACKSLASH, prev), pos + 1, line, code.tail)
        case '+' => go(Token(pos, line, PLUS, prev), pos + 1, line, code.tail)
        case '=' =>
          if (code.tail.head == '>')
            go(Token(pos, line, IMPLY, prev), pos + 2, line, code.tail.tail)
          else
            go(Token(pos, line, EQUAL, prev), pos + 1, line, code.tail)
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
              case Token(p, l, DASH, pre) if p + 1 == pos && l == line =>
                go(Token(p, l, INT(-num.toInt), pre), pos + num.length, line, code.drop(num.length))
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

    def comment(pos: Int, line: Int, code: List[Char]): (Int, Int, List[Char]) = code match {
      case '*' :: '/' :: tail => (pos + 2, line, tail)
      case '\n' :: tail => comment(1, line + 1, tail)
      case Nil => throw new TokenException(EndToken, "Un ended comment")
      case _ :: tail => comment(pos + 1, line, tail)
    }

    @tailrec
    def list(token: Tokens, l: List[(Int, Int, ProofToken)]): List[(Int, Int, ProofToken)] = token match {
      case Token(p, ln, t, pre) => list(pre, (p, ln, t) :: l)
      case EndToken => l
    }

    val l = list(go(EndToken, 1, 1, code), List.empty)
    (l :\ (EndToken:Tokens))((p, next) => Token(p._1, p._2, p._3, next))
  }

  type Read[T] = (T, Tokens)

  sealed trait Tokens {
    def token: ProofToken
    def tail: Tokens

    def position: String

    def is(token: ProofToken): Boolean = this.token == token
    def is(str: String): Boolean = is(STRING(str))
    def expect(t: ProofToken): Tokens = if (token == t) tail else throw new UnexpectedTokenException(this, t)
    def expect(str: String): Tokens = expect(STRING(str))
    def expect[T](par: Block, reader: Tokens => Read[T]): Read[T] = {
      val (e, tail) = reader(expect(par.open))
      (e, tail.expect(par.close))
    }

    def ignore(t: ProofToken): Tokens = if (token == t) tail else this
    def ignore[T](par: Block, reader: Tokens => Read[T]): Read[T] =
      if (token == par.open) {
        val (e, t1) = reader(tail)
        (e, t1.expect(par.close))
      } else reader(this)

    def string(): Read[String] = token match {
      case STRING(str) => (str, tail)
      case _ => throw new UnexpectedTokenException(this, STRING("abc"))
    }

    def int(): Read[Int] = token match {
      case INT(i) => (i, tail)
      case _ => throw new UnexpectedTokenException(this, INT(123))
    }

    def readList[T](separator: ProofToken, reader: Tokens => Read[T]): Read[List[T]] = {
      def rl(t: Tokens, acc: List[T]): Read[List[T]] = {
        val (e, tail) = reader(t)
        if (tail is separator)
          rl(tail.tail, e :: acc)
        else
          ((e :: acc).reverse, tail)
      }
      rl(this, Nil)
    }

    def readList[T](separator: ProofToken, block: Block, reader: Tokens => Read[T]): Read[List[T]] = {
      val t = expect(block.open)
      if (t is block.close)
        (List.empty, t.tail)
      else {
        val (e, tail) = t.readList(separator, reader)
        (e, tail.expect(block.close))
      }
    }

    def whileMatch[T](cond: ProofToken, reader: Tokens => Read[T]): Read[List[T]] = {
      def wm(tokens: Tokens, acc: List[T]): Read[List[T]] =
        if (tokens is cond) {
          val (e, t1) = reader(tokens)
          wm(t1, e :: acc)
        }
        else (acc.reverse, tokens)
      wm(this, Nil)
    }

    def whileNot[T](end: ProofToken, reader: Tokens => Read[T]): Read[List[T]] = {
      def wn(tokens: Tokens, acc: List[T]): Read[List[T]] =
        if (tokens is end)
          (acc.reverse, tokens)
        else {
          val (e, tail) = reader(tokens)
          wn(tail, e :: acc)
        }
      wn(this, Nil)
    }

    def when[T](cond: ProofToken, reader: Tokens => Read[T]): Read[Option[T]] =
      if (is(cond)) {
        val (e, t1) = reader(tail)
        (Some(e), t1)
      } else (None, this)

    def whenBlock[T](cond: Block, reader: Tokens => Read[T]): Read[Option[T]] =
      if (is(cond.open)) {
        val (e, t1) = reader(tail)
        (Some(e), t1.expect(cond.close))
      } else (None, this)

    def option[T](none: ProofToken, reader: Tokens => Read[T]): Read[Option[T]] =
      if (is(none)) (None, tail)
      else {
        val (e, tail) = reader(this)
        (Some(e), tail)
      }
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
  case object PLUS extends ProofToken
  case object EQUAL extends ProofToken
  case object IMPLY extends ProofToken
  case object DASH extends ProofToken
  case object ARROW extends ProofToken
  case object TILDE extends ProofToken
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
