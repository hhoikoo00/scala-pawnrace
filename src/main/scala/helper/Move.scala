package helper

class Move(
  val from: Square,
  val to: Square,
  val isCapture: Boolean,
  val isEnPassantCapture: Boolean) {

  def asSAN: String = if (!isCapture && !isEnPassantCapture) {
    // not a capture, so straight-forward moving pawn; starting square can be omitted
    s"${to.file}${to.rank}"
  } else {
    // capture move
    s"${from.file}x${to.file}${to.rank}"
  }
}