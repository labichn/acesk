package dvh.acesk

trait ClosureT[T <: Location] extends Storable[T] {
  def ll: List[T]
}

case class Closure(m: Expression, en: PosEnv) extends ClosureT[PosLoc] {
  override def toString = "<"+m+" "+en+" @ ("+m.pos.line+", "+m.pos.column+")>"
  def pos = m.pos
  def ll = en.ll
}
