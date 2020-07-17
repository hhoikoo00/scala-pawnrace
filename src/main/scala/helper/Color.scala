package helper

sealed trait Color {
  def name: String
  def opponent: Color
  def forwardMove: (Int, Int) => Int
}
object Color {
  case object BLACK extends Color {
    override def name: String = "Black"
    override def toString: String = "B"
    override def opponent: Color = WHITE
    override def forwardMove: (Int, Int) => Int = _ - _
  }
  case object WHITE extends Color {
    override def name: String = "White"
    override def toString: String = "W"
    override def opponent: Color = BLACK
    override def forwardMove: (Int, Int) => Int = _ + _
  }
  case object NONE extends Color {
    override def name: String = "None"
    override def toString: String = "."
    override def opponent: Color = throw new UnsupportedOperationException
    override def forwardMove: (Int, Int) => Int = throw new UnsupportedOperationException
  }
}
