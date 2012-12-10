package dvh.acesk

trait Environment[T <: Location, U <: Environment[T, U]] extends Function1[Var, T] {
  def bind(v: Var, l: T): U
  def domain: List[Var] = Nil
  def range: List[T] = Nil
  def ll: List[T]
}

trait PosEnv extends Environment[PosLoc, PosEnv] {
  def bind(v: Var, l: PosLoc): PosEnv = ConsEnv(v, l, this)
  def ll: List[PosLoc]
}

case object MtEnv extends PosEnv {
  def apply(v1: Var) =
    throw new RuntimeException("The variable " + v1.name + " is not in the environment.")
  override def toString = "∅"
  def ll = Nil
}

case class ConsEnv(v: Var, l: PosLoc, e: PosEnv) extends PosEnv {
  def apply(v1: Var) = if (v.name == v1.name) l else e(v1)
  override def domain = v::e.domain
  override def range = l::e.range
  override def toString = toString(this, "ρ{")
  def toString(e1: PosEnv, a: String): String = e1 match {
    case ConsEnv(v1, l1, e2) =>
      toString(e2, a+"("+v1+" -> "+l1+") ")
    case MtEnv => a.substring(0, a.length-1) + "}"
  }
  def ll = range
}
