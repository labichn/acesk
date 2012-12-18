package dvh.acesk

import scala.util.parsing.input.Position

trait State extends Storable

case class Ev(m: Expression, e: Environment, s: Store, k: Kontinuation, cn: Contour) extends State {
  def ll = e.ll:::k.ll
}
case class Co(k: Kontinuation, v: Storable, s: Store) extends State {
  def ll = k.ll:::v.ll
}
case class Ap(u: Storable, v: Storable, k: Kontinuation, s: Store, cn: Contour, lb: Label) extends State {
  def ll = u.ll:::v.ll:::k.ll
}
case class Ans(s: Store, v: Storable) extends State {
  def ll = v.ll
}
