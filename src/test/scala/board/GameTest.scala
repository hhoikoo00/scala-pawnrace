package board

import org.scalatest.funsuite.AnyFunSuite

import helper.Color._
import helper._

class GameTest extends AnyFunSuite {
  test("Game initializes correctly") {
    val testGame = new Game(new Board(7, 7))

    assertResult(None)(testGame.lastMove)
    assertResult(WHITE)(testGame.currentPlayer)
    assertResult(false)(testGame.isFinished)
  }

  test("applyMove works as intended") {
    val testBoard = new Board(7, 7)
    val testGame = new Game(testBoard)

    val move1 = new Move(
      testBoard.getSquare(1, 1),
      testBoard.getSquare(3, 1),
      false,
      false
    )
    testGame.applyMove(move1)

    assertResult(BLACK)(testGame.currentPlayer)
    assertResult(Some(move1))(testGame.lastMove)

    val move2 = new Move(
      testBoard.getSquare(6, 1),
      testBoard.getSquare(4, 1),
      false,
      false
    )
    testGame.applyMove(move2)

    assertResult(WHITE)(testGame.currentPlayer)
    assertResult(Some(move2))(testGame.lastMove)
  }

  test("unapplyMove works as intended") {
    val testBoard = new Board(7, 7)
    val testGame = new Game(testBoard)

    val move1 = new Move(
      testBoard.getSquare(1, 1),
      testBoard.getSquare(3, 1),
      false,
      false
    )
    testGame.applyMove(move1)

    val move2 = new Move(
      testBoard.getSquare(6, 1),
      testBoard.getSquare(4, 1),
      false,
      false
    )
    testGame.applyMove(move2)

    testGame.unapplyMove()

    assertResult(BLACK)(testGame.currentPlayer)
    assertResult(Some(move1))(testGame.lastMove)

    testGame.unapplyMove()

    assertResult(WHITE)(testGame.currentPlayer)
    assertResult(None)(testGame.lastMove)
  }

  test("applyMove and unapplyMove \'stress\' test") {
    val testBoard = new Board(7, 7)
    val testGame = new Game(testBoard)

    val move1 = new Move(
      testBoard.getSquare(1, 1),
      testBoard.getSquare(3, 1),
      false,
      false
    )
    val move2 = new Move(
      testBoard.getSquare(6, 1),
      testBoard.getSquare(4, 1),
      false,
      false
    )
    val move3 = new Move(
      testBoard.getSquare(4, 1),
      testBoard.getSquare(2, 1),
      false,
      false
    )

    testGame.applyMove(move1)
    testGame.applyMove(move2)
    testGame.unapplyMove()
    testGame.unapplyMove()
    testGame.applyMove(move2)
    testGame.unapplyMove()

    assertResult(None)(testGame.lastMove)
    assertResult(WHITE)(testGame.currentPlayer)

    testGame.applyMove(move2)

    assertResult(BLACK)(testGame.currentPlayer)

    testGame.applyMove(move3)

    assertResult(Some(move3))(testGame.lastMove)
    assertResult(WHITE)(testGame.currentPlayer)

    testGame.unapplyMove()

    assertResult(Some(move2))(testGame.lastMove)
    assertResult(BLACK)(testGame.currentPlayer)

    testGame.unapplyMove()

    assertResult(None)(testGame.lastMove)
    assertResult(WHITE)(testGame.currentPlayer)
  }

  test("Correctly identifies when White won") {
    val testBoard = new Board(7, 7)
    val testGame = new Game(testBoard)
    // Empty board
    testGame.allSquares(BLACK).foreach(_.occupier = NONE)
    testGame.allSquares(WHITE).foreach(_.occupier = NONE)

    // a White pawn reached the end
    testBoard.getSquare(7, 1).occupier = WHITE
    assert(testGame.isFinished)
    assertResult(WHITE)(testGame.gameResult)

    // reset
    testBoard.getSquare(7, 1).occupier = NONE
    // there are no more Black pawn on the board
    testBoard.getSquare(5, 1).occupier = WHITE
    assert(testGame.isFinished)
    assertResult(WHITE)(testGame.gameResult)
  }

  test("Correctly identifies when Black won") {
    val testBoard = new Board(7, 7)
    val testGame = new Game(testBoard)
    // Empty board
    testGame.allSquares(BLACK).foreach(_.occupier = NONE)
    testGame.allSquares(WHITE).foreach(_.occupier = NONE)

    // a Black pawn reached the end
    testBoard.getSquare(0, 1).occupier = BLACK
    assert(testGame.isFinished)
    assertResult(BLACK)(testGame.gameResult)

    // reset
    testBoard.getSquare(7, 1).occupier = NONE
    // there are no more White pawn on the board
    testBoard.getSquare(5, 1).occupier = BLACK
    assert(testGame.isFinished)
    assertResult(BLACK)(testGame.gameResult)
  }

  test("If either team no longer have any moves left the game finishes") {
    val testBoard = new Board(7, 7)
    val testGame = new Game(testBoard)

    // Empty board
    testGame.allSquares(BLACK).foreach(_.occupier = NONE)
    testGame.allSquares(WHITE).foreach(_.occupier = NONE)

    // White has no valid moves left
    testBoard.getSquare(3, 1).occupier = WHITE
    testBoard.getSquare(4, 1).occupier = BLACK
    testBoard.getSquare(4, 3).occupier = BLACK

    assert(testGame.isFinished)

    // Empty board
    testGame.allSquares(BLACK).foreach(_.occupier = NONE)
    testGame.allSquares(WHITE).foreach(_.occupier = NONE)

    // Change current player to black by making a dummy move
    testGame.applyMove(new Move(
      testBoard.getSquare(0, 0),
      testBoard.getSquare(0, 1),
      false,
      false
    ))

    // Black has no valid moves left
    testBoard.getSquare(4, 1).occupier = BLACK
    testBoard.getSquare(3, 1).occupier = WHITE
    testBoard.getSquare(3, 3).occupier = WHITE

    assert(testGame.isFinished)
  }

  test("allMoves(WHITE) and allMoves(BLACK) correct for initial board") {
    val testBoard = new Board(7, 6)
    val testGame = new Game(testBoard)

    assertResult(14)(testGame.allMoves(WHITE).size)
    assertResult(14)(testGame.allMoves(BLACK).size)
  }

  test("allMoves(WHITE) and allMoves(BLACK) stress test") {
    val board = new Board(7, 6)
    val game = new Game(board)

    // Empty the board
    game.allSquares(BLACK).foreach(_.occupier = NONE)
    game.allSquares(WHITE).foreach(_.occupier = NONE)

    // Get White diagonal's possible movements
    for (i <- 0 until 8) board.getSquare(i, i).occupier = WHITE
    assertResult(8)(game.allMoves(WHITE).size)
    assertResult(0)(game.allMoves(BLACK).size)

    // Reset and Get Black diagonal's possible movements
    for (i <- 0 until 8) board.getSquare(i, i).occupier = BLACK
    assertResult(0)(game.allMoves(WHITE).size)
    assertResult(8)(game.allMoves(BLACK).size)

    // Reset
    for (i <- 0 until 8) board.getSquare(i, i).occupier = NONE

    // Check for normal captures
    board.getSquare(2, 2).occupier = WHITE
    board.getSquare(3, 3).occupier = BLACK
    board.getSquare(2, 4).occupier = WHITE
    board.getSquare(3, 5).occupier = BLACK
    assertResult(5)(game.allMoves(WHITE).size)
    assertResult(5)(game.allMoves(BLACK).size)

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
    assertResult(2)(game.allMoves(WHITE).size)
    assertResult(2)(game.allMoves(WHITE).size)
    assertResult(1)(game.allMoves(BLACK).size)
    assert(game.allMoves(WHITE).contains(new Move(
      board.getSquare(4, 1),
      board.getSquare(5, 2),
      true,
      true
    )))

    // now apply redundant move; en passant no longer possible
    game.applyMove(new Move(
      board.getSquare(2, 1),
      board.getSquare(3, 1),
      false,
      false
    ))
    assertResult(1)(game.allMoves(WHITE).size)
    assertResult(1)(game.allMoves(BLACK).size)
    assert(!game.allMoves(WHITE).contains(new Move(
      board.getSquare(4, 1),
      board.getSquare(5, 2),
      true,
      true
    )))
  }
}
