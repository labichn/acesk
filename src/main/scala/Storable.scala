package dvh.acesk

trait Storable[T <: Location] {
  def ll: List[T]
}
