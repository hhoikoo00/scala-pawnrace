import board._
import helper.Color._
import helper.Move

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.util.Try

object TestGround extends App {
  def printApplyDisplay(move: Move): Unit = {
    println(move)
    game.applyMove(move)
    board.display()
  }

  val board = new Board(7, 6)
  val game = new Game(board)

  board.display()

  while (true) {
    print(s"${game.currentPlayer.name}'s turn: Enter SAN > ")
    var inputMove = game.parseMove(StdIn.readLine())
    while (inputMove.isEmpty) {
      print("Invalid command: reenter SAN > ")
      inputMove = game.parseMove(StdIn.readLine())
    }
    printApplyDisplay(inputMove.get)
  }


  /*
  def displayGame(): Unit = {
    board.display()
    println(s"White: ${game.whiteSquares.map(s => s"(${s.rank}, ${s.file})")}")
    println(s"White Moves: ${game.whiteMoves.map(_.asSAN)}")
    println(s"Black: ${game.blackSquares.map(s => s"(${s.rank}, ${s.file})")}")
    println(s"Black Moves: ${game.blackMoves.map(_.asSAN)}")
  }

  val board = new Board(7, 6)
  val game = new Game(board)

  displayGame()

  // Empty the board
  game.blackSquares.foreach(_.occupier = NONE)
  game.whiteSquares.foreach(_.occupier = NONE)

  displayGame()

  // Get White diagonal's possible movements
  for (i <- 0 until 8) board.getSquare(i, i).occupier = WHITE

  displayGame()

  // Reset and Get Black diagonal's possible movements
  for (i <- 0 until 8) board.getSquare(i, i).occupier = BLACK

  displayGame()

  // Reset
  for (i <- 0 until 8) board.getSquare(i, i).occupier = NONE
  // Check for normal captures
  board.getSquare(2, 2).occupier = WHITE
  board.getSquare(3, 3).occupier = BLACK
  board.getSquare(2, 4).occupier = WHITE
  board.getSquare(3, 5).occupier = BLACK

  displayGame()

  // Reset
  board.getSquare(2, 2).occupier = NONE
  board.getSquare(3, 3).occupier = NONE
  board.getSquare(2, 4).occupier = NONE
  board.getSquare(3, 5).occupier = NONE
  // Set up for en passant
  board.getSquare(4, 1).occupier = WHITE
  board.getSquare(6, 2).occupier = BLACK
  // Opponent move made; en passant now possible
  game.applyMove(new Move(
    board.getSquare(6, 2),
    board.getSquare(4, 2),
    false,
    false
  ))
  displayGame()

  // en passant capture
  game.applyMove(new Move(
    board.getSquare(4, 1),
    board.getSquare(5, 2),
    true,
    true
  ))
  displayGame()

  // reset en passant capture
  game.unapplyMove()
  // now apply redundant move;
  game.applyMove(new Move(
    board.getSquare(2, 1),
    board.getSquare(3, 1),
    false,
    false
  ))
  displayGame()
  */
}
