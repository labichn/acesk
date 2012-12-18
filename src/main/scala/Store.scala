package dvh.acesk

import scala.collection.immutable.HashMap

class Store(val map: Map[Addr, Set[Storable]]) extends Function1[Addr, Set[Storable]] {
  def alloc(l: Addr) = new Store(map + ((l, Set())))
  def apply(l: Addr) = map(l)
  def bind(l: Addr, s: Storable) =
    if (contains(l)) rebind(l, s) else new Store(map + ((l, Set(s))))
  def contains(l: Addr): Boolean = map contains l
  def domain = (map keys).toList
  def range = (map values).flatMap(_.toList).toList
  def rebind(l: Addr, s: Storable) = new Store(map + ((l, map(l)+s)))
  override def toString =
    "Store: {"+(map.toList.foldLeft("}")((a, n)=> n match {
        case (k, v) => ", "+k+" -> "+v+a
    }).substring(2))
  override def equals(that: Any) = that match {
    case s: Store => map == s.map
    case _ => false
  }
  override def hashCode = map.hashCode
}
object MtStore extends Store(new HashMap[Addr, Set[Storable]]) {
  override def toString = "Store: {}"
}
