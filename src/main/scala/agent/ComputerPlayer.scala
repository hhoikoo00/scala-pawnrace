package agent

import board._
import helper._

import scala.util.Random

class ComputerPlayer(
  game: Game, board: Board, color: Color
) extends AbstractPlayer(game, board, color) {

  override def makeMove(): Unit = {
    println(s"${game.currentPlayer.name}'s turn: computer thinking...")
    val validMoves = game.allMoves(color)
    game.applyMove(validMoves(new Random().nextInt(validMoves.length)))
  }
}