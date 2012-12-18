package dvh.acesk

import Ops._

import scala.util.parsing.input.Position

trait Kontinuation extends Storable {
  protected val sep = ", "
  protected def toString(l: List[_]): String = toString(l, "(")
  protected def toString(l: List[_], a: String): String = l match {
    case x::xs => toString(xs, a+x+sep)
    case _ =>
      if (a.length >= sep.length)
        a.substring(0, a.length-sep.length)+")"
      else
        a+")"
  }
}

case object MtKont extends Kontinuation {
  val cn: Contour = 0
  override def toString = "mt"
  def ll = Nil
}
case class Fn(v: Storable, a: Addr, cn: Contour, lb: Label) extends Kontinuation {
  override def toString = "fn("+v+sep+a+")"
  def ll = List(a)
}
case class Ar(e: Expression, env: Environment, a: Addr, cn: Contour, lb: Label) extends Kontinuation {
  override def toString = "ar("+e+sep+env+a+")"
  def ll = a::env.ll
}
case class Fi(t: Expression, e: Expression, env: Environment, a: Addr, cn: Contour) extends Kontinuation {
  override def toString = "if("+t+", "+e+", "+a+")"
  def ll = a::env.ll
}
case class Op(op: Ops,
                 vs: List[Storable],
                 ms: List[Expression],
                 e: Environment,
                 a: Addr,
                 cn: Contour,
                 lb: Label) extends Kontinuation {
  override def toString =
    "op("+Ops.toString(op)+sep+toString(vs)+sep+toString(ms)+sep+e+sep+a+")"
  def ll = a::vs.flatMap(_.ll):::e.ll
}
case class St(l: Addr, a: Addr, cn: Contour, lb: Label) extends Kontinuation {
  override def toString = "st("+l+sep+a+")"
  def ll = List(l, a)
}
case class Lr(xs: List[Var],
                 vvs: List[(Var, Storable)],
                 ms: List[Expression],
                 env: Environment,
                 n: Expression,
                 a: Addr,
                 cn: Contour,
                 lb: Label) extends Kontinuation {
  override def toString =
    "lr("+toString(xs)+sep+toString(vvs)+sep+toString(ms)+sep+env+sep+n+sep+a+")"
  def ll = a::env.ll
}
