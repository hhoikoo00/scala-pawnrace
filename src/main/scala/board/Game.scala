package board

import helper._
import helper.Color._

import scala.collection.mutable.ArrayBuffer

class Game(val board: Board) {
  private val moves = new ArrayBuffer[Move]()
  private var movesIndex = 0
  private var _currentPlayer: Color = WHITE

  def currentPlayer: Color = _currentPlayer

  def lastMove: Option[Move] = if (movesIndex < 1) None else Some(moves(movesIndex - 1))

  def applyMove(move: Move): Unit = {
    moves(movesIndex) = move
    movesIndex += 1
    board.applyMove(move)
    _currentPlayer = _currentPlayer.opponent
  }

  def unapplyMove(): Unit = {
    if (movesIndex >= 1) {
      val moveToUnapply = moves(movesIndex - 1)
      movesIndex -= 1
      board.unapplyMove(moveToUnapply)
      _currentPlayer = _currentPlayer.opponent
    }
  }

  def isFinished: Boolean = {
    // TODO implement isFinished in Game
    false
  }

  def gameResult: Color = {
    // TODO implement gameResult in Game
    NONE
  }

  def parseMove(san: String): Option[Move] = {
    // TODO implement parseMove() in Game
    None
  }
}
