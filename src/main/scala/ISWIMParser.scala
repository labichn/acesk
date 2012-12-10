package dvh.acesk

import scala.util.parsing.combinator._
import dvh.acesk.Ops._

object ISWIMParser extends JavaTokenParsers {

  override def skipWhitespace = true

  def expr: Parser[Expression] =
    positioned(letrec | if0 | value | set | oper | variable | app)

  def oparen: Parser[String] = "(" | "[" | "{"
  def cparen: Parser[String] = ")" | "]" | "}"

  def letrec: Parser[Expression] =
    positioned(oparen ~ "letrec" ~ oparen ~ rep1(lrclause) ~ cparen ~ expr ~ cparen ^^ {
      case _ ~ _ ~ _ ~ xvs ~ _ ~ m ~ _ => Letrec(xvs, m)
    })

  def lrclause: Parser[(Var, Expression)] =
    oparen ~ variable ~ expr ~ cparen ^^ {
      case _ ~ vari ~ valu ~ _ => (vari, valu)
    }

  def set: Parser[Expression] =
    positioned(oparen ~ "set" ~ variable ~ expr ~ cparen ^^ {
      case _ ~ x ~ m ~ _ => SetBang(x, m)
    })

  def if0: Parser[Expression] =
    positioned(oparen ~ "if0" ~ expr ~ expr ~ expr ~ cparen ^^ {
      case _ ~ _ ~ p ~ t ~ e ~ _ => IfZero(p, t, e)
    })

  def app: Parser[Expression] =
    positioned(oparen ~ rep1(expr) ~ cparen ^^ {
      case _ ~ (m::ms) ~ _ => appLeft(m, ms)
      case _ ~ Nil ~ _ => throw new RuntimeException("Won't occur.")
    })
  private def appLeft(m: Expression, ms: List[Expression]): Expression =
    ms.foldLeft(m)((a, n) => App(a, n))

  def oper: Parser[Oper] =
    positioned(oparen ~ primOp ~ rep1(expr) ~ cparen ^^ {
      case _ ~ o ~ ms ~ _ => Oper(o, ms)
    })
  
  def value: Parser[Value] = fun | con
  
  def variable: Parser[Var] = positioned(ident ^^ { x => Var(Symbol(x)) })
  
  // AMIRITE??
  def fun: Parser[Fun] =
    positioned(oparen ~ ("lambda" | "λ") ~ rep1(variable) ~ "." ~ rep1(expr) ~ cparen ^^ {
      case _ ~ _ ~ vs ~ _ ~ (m::ms) ~ _ => vs match {
        case w::ws => Fun(w, ws.foldRight(appLeft(m, ms))((y, n) => Fun(y, n)))
        case _ => throw new RuntimeException("Will not create a function " +
                                             "without any parameters.")
      }
      case _ => throw new RuntimeException("Will not create a function with no body.")
    })
  
  def con: Parser[Con] =
    positioned(wholeNumber ^^ { case n => Con(Integer.parseInt(n)) })

  def primOp: Parser[Ops] = (
      "add1" ^^ { case x => Ops.Add1 }
  |   "sub1" ^^ { case x => Ops.Sub1 }
  |      "-" ^^ { case x => Ops.Sub }
  |      "+" ^^ { case x => Ops.Add }
  |      "^" ^^ { case x => Ops.Exp }
  |      "*" ^^ { case x => Ops.Mul }
  | "isZero" ^^ { case x => Ops.IsZero }
  )

}
