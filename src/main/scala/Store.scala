package dvh.acesk

trait Store extends Function1[Location, Set[Storable]] {
  def alloc(l: Location): Store
  def bind(l: Location, s: Storable): Store
  def rebind(l: Location, s: Storable): Store
  def next: Location
  def domain: List[Location] = Nil
  def range: List[Storable] = Nil
}
trait ListStore extends Store {
  def alloc(l: Location) = ConsStore(l, Set.empty, ListStore.this)
  def bind(l: Location, s: Storable) = ConsStore(l, Set(s), ListStore.this)
  def rebind(l: Location, s: Storable): ListStore
}
case object EmptyStore extends ListStore {
  def apply(l: Location) =
    throw new RuntimeException("No location ("+l+") in this store.")
  def next = Location(0)
  def rebind(l: Location, s: Storable): ListStore =
    throw new RuntimeException("No location ("+l+") in this store " +
                               "to rebind to "+s+".")
  override def toString = "mt"
}
case class ConsStore(l: Location, ss: Set[Storable], s: ListStore) extends ListStore {
  override def domain: List[Location] = l::s.domain
  override def range: List[Storable] = ss.toList:::s.range
  def apply(l1: Location) = if (l == l1) ss else s(l1)
  def next = Location(scala.math.max(l.n+1, s.next.n))
  def rebind(l1: Location, s1: Storable) =
    if (l1 == l) ConsStore(l, ss+s1, s)
    else ConsStore(l, ss, s.rebind(l1, s1))
/*  override def toString = toString(this, "σ{")
  def toString(s1: ListStore, a: String): String = s1 match {
    case ConsStore(l1, c1, s2) =>
      toString(s2, a+"("+l1+" -> "+(c1 match {
        case Some(c) => c
        case _ => "␀"
      })+") ")
    case EmptyStore =>
      a.substring(0, a.length-1) + "}"
  }*/
}
