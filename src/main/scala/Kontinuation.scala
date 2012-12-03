package dvh.acesk

import Ops._

trait Kontinuation extends Storable {
  val sep = ", "
  def ll: List[Location]
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
case object EmptyKon extends Kontinuation {
  override def toString = "mt"
  def ll = Nil
}
case class Fn(v: Closure, a: Location) extends Kontinuation {
  override def toString = "fn("+v+sep+"κ)"
  def ll = a::v.ll
}
case class Ar(c: Closure, a: Location) extends Kontinuation {
  override def toString = "ar("+c+sep+"κ)"
  def ll = a::c.ll
}
case class If(env: Environment, t: Expression, e: Expression, a: Location) extends Kontinuation {
  override def toString = "if("+t+", "+e+", "+a+")"
  def ll = a::env.ll
}
case class Op(op: Ops,
              vs: List[Closure],
              cs: List[Closure],
              a: Location) extends Kontinuation {
  override def toString =
    "op("+Ops.toString(op)+", "+toString(vs)+sep+toString(cs)+", κ)"
  def ll = a::vs.flatMap(_.ll):::cs.flatMap(_.ll)
}
case class St(l: Location, a: Location) extends Kontinuation {
  override def toString = "st("+l+sep+"κ)"
  def ll = List(l, a)
}
case class Lr(xs: List[Var],
              vvs: List[(Var, Value)],
              ms: List[Expression],
              e: Environment,
              n: Expression,
              a: Location) extends Kontinuation {
  override def toString =
    "lr("+toString(xs)+sep+toString(vvs)+sep+toString(ms)+sep+e+sep+n+sep+"κ)"
  def ll = a::e.ll
}
