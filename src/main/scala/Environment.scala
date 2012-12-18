package dvh.acesk

import scala.collection.immutable.HashMap

class Environment(val map: Map[Var, Addr]) extends Function1[Var, Addr] {
  def apply(v: Var) = map(v)
  def bind(v: Var, l: Addr) = new Environment(map + ((v, l)))
  def contains(v: Var) = map contains v
  def domain = (map keys).toList
  def range = (map values).toList
  def ll = range
  override def toString =
    "Env: {"+(map.toList.foldLeft("}")((a, n)=> n match {
        case (k, v) => ", "+k+" -> "+v+a
    }).substring(2))
  override def equals(that: Any) = that match {
    case e: Environment => map == e.map
    case _ => false
  }
  override def hashCode = map.hashCode
}
object MtEnv extends Environment(new HashMap[Var, Addr]) {
  override def toString = "Env: {}"
}
