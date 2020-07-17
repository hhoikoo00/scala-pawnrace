package board

import helper._
import helper.Color._

import scala.collection.mutable
import scala.util.Try

class Game(board: Board) {
  private val moves = mutable.Map[Int, Move]()
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

  def allSquares(color: Color): Seq[Square] =
    board.board.flatMap(_.filter(_.occupier == color)).toSeq

  def allMoves(color: Color): Seq[Move] = allSquares(color).flatMap(squareMoves)

  private def canEnPassant(square: Square): Boolean = lastMove match {
    case Some(move) =>
      !move.isCapture && !move.isEnPassantCapture && // Not a capture
        move.from.x == Board.getInitRow(move.to.occupier) && // Move is from initial rank
        Math.abs(move.from.x - move.to.x) == 2 && // Moved by 2 ranks
        move.from.y == move.to.y && // Move still in same file
        move.to.x == square.x && // square and destination in same rank
        Math.abs(move.from.y - square.y) == 1 // move and square in adjacent file
    case None => false
  }

  private def squareMoves(square: Square): Seq[Move] = {
    val moves = new mutable.ArrayBuffer[Move]()

    val forwardMove = square.occupier.forwardMove

    val xFwd = forwardMove(square.x, 1)
    val x2Fwd = forwardMove(square.x, 2)

    /* Try Monad: execute only when fwdSquare reachable */
    for (fwdSquare <- Try(board.getSquare(xFwd, square.y))) {
      /** Normal Forward */
      if (fwdSquare.occupier == NONE)
        moves += new Move(square, fwdSquare, false, false)

      /* Try Monad: execute only when fwd2Square reachable */
      for (fwd2Square <- Try(board.getSquare(x2Fwd, square.y))) {
        /** Initial Forward */
        val isAtStart = square.x == Board.getInitRow(square.occupier)
        if (isAtStart && fwdSquare.occupier == NONE && fwd2Square.occupier == NONE)
          moves += new Move(square, fwd2Square, false, false)
      }
    }

    /* for each valid file(column) to capture */
    Seq(square.y - 1, square.y + 1).filter(y => 0 <= y && y < 8).foreach(y => {
      /* Try Monad: execute only when captureSquare reachable */
      for (captureSquare <- Try(board.getSquare(xFwd, y))) {
        /** Normal Capture */
        if (captureSquare.occupier == square.occupier.opponent)
          moves += new Move(square, captureSquare, true, false)

        /** En Passant Capture */
        /* Option Monad: execute only when lastMove exists */
        for (move <- lastMove)
          if (canEnPassant(square) && move.to.y == y)
            moves += new Move(square, captureSquare, true, true)
      }
    })

    moves.toSeq
  }

  private def blackWins: Boolean =
    board.board(Board.blackEnd).exists(_.occupier == BLACK) || allSquares(WHITE).isEmpty

  private def whiteWins: Boolean =
    board.board(Board.whiteEnd).exists(_.occupier == WHITE) || allSquares(BLACK).isEmpty

  def isFinished: Boolean = blackWins || whiteWins ||
    (currentPlayer == BLACK && allMoves(BLACK).isEmpty) ||
    (currentPlayer == WHITE && allMoves(WHITE).isEmpty)

  def gameResult: Color = if (blackWins) BLACK else if (whiteWins) WHITE else NONE

  def parseMove(san: String): Option[Move] =
    if (san.length == 2) // Maybe a simple move
      for {
        // Parse input string into x and y coordinate
        destY <- Option(san(0)).map(_.toLower - 'a').filter(i => 0 <= i && i < 8)
        destX <- Option(san(1)).map(_.asDigit - 1).filter(i => 0 <= i && i < 8)

        destSquare = board.getSquare(destX, destY)
        // Only when the destination square is empty
        _ <- Option.when(destSquare.occupier == NONE)()

        // Find source square based on destination
        srcY = destY
        destBack1Square <- Try(board.getSquare(currentPlayer.forwardMove(destX, -1), srcY)).toOption
        destBack2Square <- Try(board.getSquare(currentPlayer.forwardMove(destX, -2), srcY)).toOption

        // Check for 1 rank behind THEN 2 ranks behind (AND destination 2 ranks after init)
        srcSquare <- if (destBack1Square.occupier == currentPlayer)
          Some(destBack1Square)
        else if (destBack2Square.occupier == currentPlayer &&
          currentPlayer.forwardMove(Board.getInitRow(currentPlayer), 2) == destX)
          Some(destBack2Square)
        else
          None
      } yield new Move(srcSquare, destSquare, false, false)
    else if (san.length == 4) // Maybe a capture
      for {
        // second character is 'x' or 'X'
        _ <- Option(san(1)).filter(_.toLower == 'x')

        // Parse destination characters
        destY <- Option(san(2)).map(_.toLower - 'a').filter(i => 0 <= i && i < 8)
        destX <- Option(san(3)).map(_.asDigit - 1).filter(i => 0 <= i && i < 8)
        destSquare = board.getSquare(destX, destY)

        // Parse source file (y coordinate)
        srcY <- Option(san(0)).map(_.toLower - 'a').filter(i => 0 <= i && i < 8)
          .filter(i => Math.abs(i - destY) == 1)
        // source square only when current player is on it
        srcSquare <- Try(board.getSquare(currentPlayer.forwardMove(destX, -1), srcY))
          .filter(_.occupier == currentPlayer).toOption

        // Destination is either opponent or NONE (and one forward opponent)
        // otherwise it is not valid
        isEnPassantCapture <- if (destSquare.occupier == currentPlayer.opponent)
          Some(false)
        else if (destSquare.occupier == NONE && canEnPassant(srcSquare))
          Some(true)
        else
          None
      } yield new Move(srcSquare, destSquare, true, isEnPassantCapture)
    else // Definitely not in SAN
      None
}
