package agent

import board._
import helper._

abstract class AbstractPlayer(val game: Game, val board: Board, val color: Color) {
  var opponent: AbstractPlayer = _

  def allPawns: Seq[Square] = game.allSquares(color)

  def allValidMoves: Seq[Move] = game.allMoves(color)

  def isPassedPawn(square: Square): Boolean = ??? //TODO implement isPassedPawn

  def makeMove(): Unit
}
