package dvh.acesk

import scala.util.parsing.input.Position

trait Location

case class IntLoc(n: Int) extends Location {
  override def toString = "["+n+"]"
}

object PosLoc {
  object dummyPos extends Position {
    def line = -1; def column = -1
    protected def lineContents: String = ""
  }
  def dummy: PosLoc = new PosLoc(dummyPos)
}

case class PosLoc(p: Position) extends Location {
  override def toString = "["+p.line+","+p.column+"]"
  override def equals(a: Any) = a match {
    case PosLoc(p1) => p1.line == p.line && p1.column == p.column
    case _ => false
  }
}
