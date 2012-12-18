package dvh.acesk

import scala.util.parsing.input.Position

trait Labeled {
  protected var label: Option[Position] = None
  def lb = label.get
  def lb(lb: Position): this.type = { label = Some(lb); this; }
}
