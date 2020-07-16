package helper

class Square(val x: Int, val y: Int, var occupier: Color = Color.NONE) {
  require(0 <= x && x < 8 && 0 <= y && y < 8, "x and y must be in range of 0-7 inclusive")

  val rank: Char = (x + 1).toString.charAt(0)
  val file: Char = ('a' + y).toChar

  override def toString: String = occupier.toString
}