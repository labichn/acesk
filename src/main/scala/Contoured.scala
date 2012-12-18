package dvh.acesk

trait Contoured {
  protected var contour: Option[Int] = None
  def cn(cn: Int): this.type = { contour = Some(cn); this; }
  def cn = contour.get
}
