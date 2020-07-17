package helper

class Move(
  val from: Square,
  val to: Square,
  val isCapture: Boolean,
  val isEnPassantCapture: Boolean) extends Equals {

  def asSAN: String = if (!isCapture && !isEnPassantCapture) {
    // not a capture, so straight-forward moving pawn; starting square can be omitted
    s"${to.file}${to.rank}"
  } else {
    // capture move
    s"${from.file}x${to.file}${to.rank}"
  }

  override def toString: String = asSAN

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Move]
  override def equals(obj: Any): Boolean = obj match {
    case that: Move =>
      that.canEqual(this) &&
      from == that.from &&
      to == that.to &&
      isCapture == that.isCapture &&
      isEnPassantCapture == that.isEnPassantCapture
    case _ => false
  }
}