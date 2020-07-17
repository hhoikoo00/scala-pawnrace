package agent

import scala.io.StdIn

import board._
import helper._

class HumanPlayer(
  game: Game, board: Board, color: Color
) extends AbstractPlayer(game, board, color) {

  override def makeMove(): Unit = {
    print(s"${game.currentPlayer.name}'s turn: Enter SAN > ")
    var inputMove = game.parseMove(StdIn.readLine())
    while (inputMove.isEmpty) {
      print("Invalid command: reenter SAN > ")
      inputMove = game.parseMove(StdIn.readLine())
    }
    game.applyMove(inputMove.get)
  }
}
