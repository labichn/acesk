package dvh.cek

import scala.annotation.tailrec

trait Expression {
  override def toString = prettyString
  def prettyString: String
  def av: List[Var]
  def fv: List[Var]
}
case class Letrec(xms: List[(Var, Expression)], n: Expression) extends Expression {
  def prettyString = "(letrec {"+toString(xms)+"} "+n+")"
  @tailrec private def toString(xms: List[(Var, Expression)], a: String = ""): String =
    xms match {
      case (x, m)::yms => toString(yms, a+"("+x+", "+m+") ")
      case _ => a.substring(0, a.length-1).toString
    }
  def av = (xms.flatMap(_._2.av):::n.av).filter(xms.map(_._1).contains(_))
  def fv = (xms.flatMap(_._2.fv):::n.fv).filter(xms.map(_._1).contains(_))
}
case class App(m: Expression, n: Expression) extends Expression {
  def prettyString = "("+m+" "+n+")"
  def av = m.av:::n.av
  def fv = m.fv:::n.fv
}
case class Oper(o: Ops.Value, ms: List[Expression]) extends Expression {
  def prettyString = {
    val tmp = "("+Ops.toString(o)+" "+ms.foldRight("")(_+" "+_)
    tmp.substring(0, tmp.length-1) + ")"
  }
  def av = ms flatMap { _.av }
  def fv = ms flatMap { _.fv }
}
case class Var(name: Symbol) extends Expression {
  def prettyString = name.name
  def av = Nil
  def fv = List(this)
}
case class SetBang(x: Var, m: Expression) extends Expression {
  def prettyString = "(set "+x+" "+m+")"
  def av = x::m.av
  def fv = x::m.fv
}
case class IfZero(p: Expression, t: Expression, e: Expression) extends Expression {
  def prettyString = "(if0 "+p+" "+t+" "+e+")"
  def av = p.av:::t.av:::e.av
  def fv = p.fv:::t.fv:::e.fv
}

trait Value extends Expression
case class Fun(x: Var, m: Expression) extends Value {
  def prettyString = "(λ"+x+"."+m+")"
  def av = m.av.filter(_ != x)
  def fv = m.fv.filter(_ != x)
}
case class Con(value: Double) extends Value {
  def prettyString =
    if (value < Int.MaxValue)
      value.asInstanceOf[Int].toString
    else
      value.toString
  def av = Nil
  def fv = Nil
}
case object Num extends Value {
  def prettyString = "allyournumarebelongtous"
  def av = Nil
  def fv = Nil
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
