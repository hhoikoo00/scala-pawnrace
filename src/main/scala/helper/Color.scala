package helper

sealed trait Color {
  def opponent: Color
}
object Color {
  case object BLACK extends Color {
    override def toString: String = "B"
    override def opponent: Color = WHITE
  }
  case object WHITE extends Color {
    override def toString: String = "W"
    override def opponent: Color = BLACK
  }
  case object NONE extends Color {
    override def toString: String = "."
    override def opponent: Color = NONE
  }
}
