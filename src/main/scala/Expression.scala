package dvh.acesk

import scala.annotation.tailrec
import scala.util.parsing.input.{Position, Positional}

trait Expression extends Positional with Labeled {
  override def toString = prettyString
  def prettyString: String
  def av: List[Var] = Nil
  def fv: List[Var] = Nil
  override def setPos(p: Position) = { super.setPos(p); label = Some(p); this }
}
case class Var(name: String) extends Expression {
  def prettyString = name
  override def fv = List(this)
}
case class Closure(x: Var,
                   body: Expression,
                   env: Environment) extends Literal {
  def prettyString = "closure"
  override def av = body.av.filter(_ != x)
  override def fv = body.fv.filter(_ != x)
  override def ll = env.ll
}
trait Literal extends Expression with Storable {
  def ll: List[Addr] = Nil
}
case class Con(value: Double) extends Literal {
  def prettyString =
    if (value < Int.MaxValue)
      value.asInstanceOf[Int].toString
    else
      value.toString
}
case object Num extends Literal {
  def prettyString = "ℕ"
}
case class Oper(o: Ops.Value, ms: List[Expression]) extends Expression {
  def prettyString = {
    val tmp = "("+Ops.toString(o)+" "+ms.foldRight("")(_+" "+_)
    tmp.substring(0, tmp.length-1) + ")"
  }
  override def av = ms flatMap { _.av }
  override def fv = ms flatMap { _.fv }
}
object Ops extends Enumeration {
  type Ops = Value
  val Add1, Sub1, IsZero, Add, Sub, Mul, Exp = Value
  def toString(v: Value) = v match {
    case Add1   => "add1"
    case Sub1   => "sub1"
    case IsZero => "isZero"
    case Add    => "+"
    case Sub    => "-"
    case Mul    => "*"
    case Exp    => "^"
  }
}
case class Lam(x: Var, body: Expression) extends Expression {
  def prettyString = "(λ"+x+"."+body+")"
  override def av = body.av.filter(_ != x)
  override def fv = body.fv.filter(_ != x)
}
case class App(m: Expression, n: Expression) extends Expression {
  def prettyString = "("+m+" "+n+")"
  override def av = m.av:::n.av
  override def fv = m.fv:::n.fv
}
case class IfZero(p: Expression, t: Expression, e: Expression) extends Expression {
  def prettyString = "(if0 "+p+" "+t+" "+e+")"
  override def av = p.av:::t.av:::e.av
  override def fv = p.fv:::t.fv:::e.fv
}
case class Letrec(xms: List[(Var, Expression)], n: Expression) extends Expression {
  def prettyString = "(letrec {"+toString(xms)+"} "+n+")"
  @tailrec private def toString(xms: List[(Var, Expression)], a: String = ""): String =
    xms match {
      case (x, m)::yms => toString(yms, a+"("+x+", "+m+") ")
      case _ => a.substring(0, a.length-1).toString
    }
  override def av = (xms.flatMap(_._2.av):::n.av).filter(xms.map(_._1).contains(_))
  override def fv = (xms.flatMap(_._2.fv):::n.fv).filter(xms.map(_._1).contains(_))
}
case class SetBang(x: Var, m: Expression) extends Expression {
  def prettyString = "(set "+x+" "+m+")"
  override def av = x::m.av
  override def fv = x::m.fv
}
