package dvh.cek

trait Environment extends Function1[Var, Location] {
  def bind(v: Var, l: Location): Environment
  def domain: List[Var] = Nil
  def range: List[Location] = Nil
  def ll: List[Location]
}
trait ListEnv extends Environment {
  def bind(v: Var, l: Location): ListEnv = ConsEnv(v, l, this)
  def ll: List[Location]
}
case object EmptyEnv extends ListEnv {
  def apply(v1: Var) =
    throw new RuntimeException("The variable " + v1.name + " is not in the environment.")
  override def toString = "∅"
  def ll = Nil
}
case class ConsEnv(v: Var, l: Location, e: ListEnv) extends ListEnv {
  def apply(v1: Var) = if (v.name == v1.name) l else e(v1)
  override def domain = v::e.domain
  override def range = l::e.range
  override def toString = toString(this, "ρ{")
  def toString(e1: ListEnv, a: String): String = e1 match {
    case ConsEnv(v1, l1, e2) =>
      toString(e2, a+"("+v1+" -> "+l1+") ")
    case EmptyEnv => a.substring(0, a.length-1) + "}"
  }
  def ll = range
}
