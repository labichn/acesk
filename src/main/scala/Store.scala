package dvh.acesk

trait Store[T <: Location, U <: Store[T, U]] extends Function1[T, Set[Storable[T]]] {
  def alloc(l: T): U
  def bind(l: T, s: Storable[T]): U
  def rebind(l: T, s: Storable[T]): U
  def domain: List[T] = Nil
  def range: List[Storable[T]] = Nil
}
trait PosStore extends Store[PosLoc, PosStore] {
  def alloc(l: PosLoc) = ConsStore(l, Set.empty, PosStore.this)
  def bind(l: PosLoc, s: Storable[PosLoc]) =
    if (contains(l)) rebind(l, s) else ConsStore(l, Set(s), PosStore.this)
  def contains(l: PosLoc): Boolean
  def rebind(l: PosLoc, s: Storable[PosLoc]): PosStore
}
case object MtStore extends PosStore {
  def contains(l: PosLoc) = false
  def apply(l: PosLoc) =
    throw new RuntimeException("No location ("+l+") in this store.")
  def rebind(l: PosLoc, s: Storable[PosLoc]): PosStore =
    throw new RuntimeException("No location ("+l+") in this store " +
                               "to rebind to "+s+".")
  override def toString = "mt"
}
case class ConsStore(l: PosLoc, ss: Set[Storable[PosLoc]], s: PosStore) extends PosStore {
  def contains(l1: PosLoc) = l == l1 || s.contains(l1)
  override def domain: List[PosLoc] = l::s.domain
  override def range: List[Storable[PosLoc]] = ss.toList:::s.range
  def apply(l1: PosLoc) = if (l == l1) ss else s(l1)
  def rebind(l1: PosLoc, s1: Storable[PosLoc]) =
    if (l1 == l) ConsStore(l, ss+s1, s)
    else ConsStore(l, ss, s.rebind(l1, s1))
  override def toString = toString(this, "σ{")
  def toString(s1: PosStore, a: String): String = s1 match {
    case ConsStore(l1, c1, s2) =>
      toString(s2, a+"("+l1+" -> "+(c1 match {
        case s: Set[Storable[PosLoc]] => s.toString
        case _ => "␀"
      })+") ")
    case MtStore =>
      a.substring(0, a.length-1) + "}"
  }
}
