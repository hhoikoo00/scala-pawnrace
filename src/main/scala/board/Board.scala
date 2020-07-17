package board

import helper._
import helper.Color._

object Board {
  val whiteRow = 1
  val blackRow = 6
  val whiteEnd = 7
  val blackEnd = 0

  def getInitRow(color: Color): Int = if (color == BLACK) blackRow else whiteRow
}

class Board(val whiteGap: Int, val blackGap: Int) {
  require(0 <= whiteGap && whiteGap < 8 && 0 <= blackGap && blackGap < 8)

  type Row = Array[Square]
  type BoardType = Array[Row]

  // Initialize empty board
  val board: BoardType = Array.ofDim(8, 8)
  for {
    i <- 0 to 7
    j <- 0 to 7
  } board(i)(j) = new Square(i, j)

  // Populate the board with both players' pawns except for whiteGap and blackGap
  for (i <- 0 to 7 if i != whiteGap) getSquare(Board.whiteRow, i).occupier = WHITE
  for (i <- 0 to 7 if i != blackGap) getSquare(Board.blackRow, i).occupier = BLACK

  def getSquare(x: Int, y: Int): Square = board(x)(y)

  def applyMove(move: Move): Unit = {
    // set the destination square to the colour moved
    move.to.occupier = move.from.occupier
    // set the source square to NONE (pawn moved)
    move.from.occupier = NONE

    if (move.isEnPassantCapture) {
      val newX = move.to.occupier match {
        case BLACK => move.to.x + 1   // White captured
        case WHITE => move.to.x - 1   // Black captured
        case _ => move.to.x           // Edge case: should not reach
      }
      // Mark "captured" pawn (in a different position) as none
      getSquare(newX, move.to.y).occupier = NONE
    }
  }

  def unapplyMove(move: Move): Unit = {
    val movedPawn = move.to.occupier
    val capturedPawn = movedPawn.opponent

    // unmove a moved pawn
    move.from.occupier = movedPawn
    // unmark the position where the pawn moved to
    move.to.occupier = NONE

    if (move.isEnPassantCapture) {
      val newX = movedPawn match {
        case BLACK => move.to.x + 1   // White captured
        case WHITE => move.to.x - 1   // Black captured
        case _ => move.to.x           // Edge case: should not reach
      }
      // remark "captured" pawn
      getSquare(newX, move.to.y).occupier = capturedPawn
    } else if (move.isCapture) {
      // remark "captured" pawn
      move.to.occupier = capturedPawn
    }
  }

  def display(): Unit = {
    println("   A B C D E F G H   \n")
    for (i <- 7 to 0 by -1) displayRow(i)
    println("\n   A B C D E F G H   ")
  }

  private[this] def displayRow(index: Int): Unit =
    println(s"${index + 1}  ${board(index).map(_.toString).reduce(_ + " " + _)}  ${index + 1}")
}

