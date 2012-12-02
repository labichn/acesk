package dvh.cek

case class Closure(m: Expression, en: Environment) extends Storable {
  override def toString = "<"+m+" "+en+">"
  def ll = en.ll
}
