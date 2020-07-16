package board

import org.scalatest.funsuite.AnyFunSuite

import helper._
import helper.Color._

class BoardTest extends AnyFunSuite {

  test("board has correct number of black and white pawns") {
    val testBoard = new Board(0, 0)

    var numWhite = 0
    var numBlack = 0
    for (i <- 0 to 7; j <- 0 to 7) {
      testBoard.getSquare(i, j).occupier match {
        case Color.WHITE => numWhite += 1
        case Color.BLACK => numBlack += 1
        case _ =>
      }
    }

    assertResult(7)(numWhite)
    assertResult(7)(numBlack)
  }

  test("board correctly places the empty pawns") {
    var white = 0
    var black = 0
    var testBoard = new Board(white, black)
    assertResult(Color.NONE)(testBoard.getSquare(Board.whiteRow, white).occupier)
    assertResult(Color.NONE)(testBoard.getSquare(Board.blackRow, black).occupier)

    white = 4
    black = 6
    testBoard = new Board(white, black)
    assertResult(Color.NONE)(testBoard.getSquare(Board.whiteRow, white).occupier)
    assertResult(Color.NONE)(testBoard.getSquare(Board.blackRow, black).occupier)

    white = 7
    black = 2
    testBoard = new Board(white, black)
    assertResult(Color.NONE)(testBoard.getSquare(Board.whiteRow, white).occupier)
    assertResult(Color.NONE)(testBoard.getSquare(Board.blackRow, black).occupier)
  }

  test("invalid parameters throw IllegalArgumentException") {
    assertThrows[IllegalArgumentException](new Board(-1, 2))
    assertThrows[IllegalArgumentException](new Board(1, -2))
    assertThrows[IllegalArgumentException](new Board(8, 2))
    assertThrows[IllegalArgumentException](new Board(1, 8))
  }

  test("applyMove for normal forward move works") {
    val testBoard = new Board(7, 7)

    /** Test for WHITE Pawn moving */
    val moveWhite = new Move(
      testBoard.getSquare(1, 2),
      testBoard.getSquare(3, 2),
      false,
      false
    )
    testBoard.applyMove(moveWhite)

    assertResult(NONE)(testBoard.getSquare(1, 2).occupier)
    assertResult(WHITE)(testBoard.getSquare(3, 2).occupier)

    /** Test for BLACK Pawn moving */
    val moveBlack = new Move(
      testBoard.getSquare(6, 3),
      testBoard.getSquare(4, 3),
      false,
      false
    )
    testBoard.applyMove(moveBlack)

    assertResult(NONE)(testBoard.getSquare(6, 3).occupier)
    assertResult(BLACK)(testBoard.getSquare(4, 3).occupier)
  }

  test("unapplyMove for normal forward move works") {
    val testBoard = new Board(7, 7)

    /** Test for WHITE Pawn moving */
    val moveWhite = new Move(
      testBoard.getSquare(1, 2),
      testBoard.getSquare(3, 2),
      false,
      false
    )
    testBoard.applyMove(moveWhite)
    testBoard.unapplyMove(moveWhite)

    assertResult(WHITE)(testBoard.getSquare(1, 2).occupier)
    assertResult(NONE)(testBoard.getSquare(3, 2).occupier)

    /** Test for BLACK Pawn moving */
    val moveBlack = new Move(
      testBoard.getSquare(6, 3),
      testBoard.getSquare(4, 3),
      false,
      false
    )
    testBoard.applyMove(moveBlack)
    testBoard.unapplyMove(moveBlack)

    assertResult(BLACK)(testBoard.getSquare(6, 3).occupier)
    assertResult(NONE)(testBoard.getSquare(4, 3).occupier)
  }

  test("applyMove for normal captures works") {
    val testBoard = new Board(7, 7)

    /** Set up a fictitious board where a capture can happen */
    testBoard.getSquare(3, 1).occupier = WHITE
    testBoard.getSquare(1, 1).occupier = NONE
    testBoard.getSquare(4, 2).occupier = BLACK
    testBoard.getSquare(6, 2).occupier = NONE

    /** Capture */
    val move = new Move(
      testBoard.getSquare(3, 1),
      testBoard.getSquare(4, 2),
      true,
      false
    )
    testBoard.applyMove(move)

    /** Test */
    assertResult(WHITE)(testBoard.getSquare(4, 2).occupier)
    assertResult(NONE)(testBoard.getSquare(3, 1).occupier)
  }

  test("unapplyMove for normal captures works") {
    val testBoard = new Board(7, 7)

    /** Set up a fictitious board where a capture can happen */
    testBoard.getSquare(3, 1).occupier = WHITE
    testBoard.getSquare(1, 1).occupier = NONE
    testBoard.getSquare(4, 2).occupier = BLACK
    testBoard.getSquare(6, 2).occupier = NONE

    /** Capture */
    val move = new Move(
      testBoard.getSquare(3, 1),
      testBoard.getSquare(4, 2),
      true,
      false
    )
    testBoard.applyMove(move)
    testBoard.unapplyMove(move)

    /** Test */
    assertResult(BLACK)(testBoard.getSquare(4, 2).occupier)
    assertResult(WHITE)(testBoard.getSquare(3, 1).occupier)
  }

  test("applyMove for en passant captures works") {
    val testBoard = new Board(7, 7)

    /** WHITE captures BLACK */
    /** Set up a fictitious board where an en passant capture can happen */
    testBoard.getSquare(4, 1).occupier = WHITE
    testBoard.getSquare(1, 1).occupier = NONE
    testBoard.getSquare(4, 2).occupier = BLACK
    testBoard.getSquare(6, 2).occupier = NONE

    /** Capture */
    var move = new Move(
      testBoard.getSquare(4, 1),
      testBoard.getSquare(5, 2),
      true,
      true
    )
    testBoard.applyMove(move)

    /** Test */
    assertResult(NONE)(testBoard.getSquare(4, 1).occupier)
    assertResult(WHITE)(testBoard.getSquare(5, 2).occupier)
    assertResult(NONE)(testBoard.getSquare(4, 3).occupier)

    /** BLACK captures WHITE */
    /** Reset testBoard */
    testBoard.getSquare(6, 2).occupier = BLACK
    testBoard.getSquare(5, 2).occupier = NONE
    testBoard.getSquare(1, 1).occupier = WHITE

    /** Set up a fictitious board where an en passant capture can happen */
    testBoard.getSquare(3, 1).occupier = WHITE
    testBoard.getSquare(1, 1).occupier = NONE
    testBoard.getSquare(3, 2).occupier = BLACK
    testBoard.getSquare(6, 2).occupier = NONE

    /** Capture */
    move = new Move(
      testBoard.getSquare(3, 2),
      testBoard.getSquare(2, 1),
      true,
      true
    )
    testBoard.applyMove(move)

    /** Test */
    assertResult(NONE)(testBoard.getSquare(3, 1).occupier)
    assertResult(BLACK)(testBoard.getSquare(2, 1).occupier)
    assertResult(NONE)(testBoard.getSquare(3, 2).occupier)
  }

  test("unapplyMove for en passant captures works") {
    val testBoard = new Board(7, 7)

    /** WHITE captures BLACK */
    /** Set up a fictitious board where an en passant capture can happen */
    testBoard.getSquare(4, 1).occupier = WHITE
    testBoard.getSquare(1, 1).occupier = NONE
    testBoard.getSquare(4, 2).occupier = BLACK
    testBoard.getSquare(6, 2).occupier = NONE

    /** Capture */
    var move = new Move(
      testBoard.getSquare(4, 1),
      testBoard.getSquare(5, 2),
      true,
      true
    )
    testBoard.applyMove(move)
    testBoard.unapplyMove(move)

    /** Test */
    assertResult(WHITE)(testBoard.getSquare(4, 1).occupier)
    assertResult(NONE)(testBoard.getSquare(5, 2).occupier)
    assertResult(BLACK)(testBoard.getSquare(4, 2).occupier)

    /** BLACK captures WHITE */
    /** Reset testBoard */
    testBoard.getSquare(4, 1).occupier = NONE
    testBoard.getSquare(4, 2).occupier = NONE

    /** Set up a fictitious board where an en passant capture can happen */
    testBoard.getSquare(3, 1).occupier = WHITE
    testBoard.getSquare(1, 1).occupier = NONE
    testBoard.getSquare(3, 2).occupier = BLACK
    testBoard.getSquare(6, 2).occupier = NONE

    /** Capture */
    move = new Move(
      testBoard.getSquare(3, 2),
      testBoard.getSquare(2, 1),
      true,
      true
    )
    testBoard.applyMove(move)
    testBoard.unapplyMove(move)

    /** Test */
    assertResult(WHITE)(testBoard.getSquare(3, 1).occupier)
    assertResult(NONE)(testBoard.getSquare(2, 1).occupier)
    assertResult(BLACK)(testBoard.getSquare(3, 2).occupier)
  }
}
