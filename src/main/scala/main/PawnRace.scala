package main

import agent._
import board._
import helper._
import helper.Color._

import scala.io.StdIn

object PawnRace {
  def filterPlayer(
    arg: String, game: Game, board: Board, color: Color
  ): Option[AbstractPlayer] = arg.toUpperCase match {
    case "P" => Some(new HumanPlayer(game, board, color))
    case "C" => Some(new ComputerPlayer(game, board, color))
    case _ => None
  }

  def filterGap(arg: String): Option[Int] = if (arg.length == 1)
    Some(arg(0).toUpper).map(_ - 'A').filter(i => 0 <= i && i < 8)
  else
    None

  def main(args: Array[String]): Unit = {
    /** Get white and black player */
    print("Is White player a computer (\"C\") or a human (\"P\")? > ")
    val whitePlayerInput = StdIn.readLine()
    print("Is Black player a computer (\"C\") or a human (\"P\")? > ")
    val blackPlayerInput = StdIn.readLine()

    /** Get gaps */
    print("Black player, where do you want to place White's gap (A-H)? > ")
    val whiteGapInput = StdIn.readLine()
    print("Black player, where do you want to place Black's gap (A-H)? > ")
    val blackGapInput = StdIn.readLine()

    /** Parse and init board */
    val board = (for {
      whiteGap <- filterGap(whiteGapInput)
      blackGap <- filterGap(blackGapInput)
    } yield new Board(whiteGap, blackGap)).getOrElse({
      Console.err.println("Invalid white gap and/or black gap")
      sys.exit(1)
    })

    /** Init game with board */
    val game = new Game(board)

    /** Init players */
    val players = (for {
      whitePlayer <- filterPlayer(whitePlayerInput, game, board, WHITE)
      blackPlayer <- filterPlayer(blackPlayerInput, game, board, BLACK)
    } yield {
      whitePlayer.opponent = blackPlayer
      blackPlayer.opponent = whitePlayer
      Map[Color, AbstractPlayer](WHITE -> whitePlayer, BLACK -> blackPlayer)
    }).getOrElse({
      Console.err.println("Invalid white player and/or black player")
      sys.exit(2)
    })

    /** actually play the game */
    while (!game.isFinished) {
      board.display()
      players(game.currentPlayer).makeMove()
    }

    /** display result */
    board.display()
    game.gameResult match {
      case NONE => println("Game ended in a stalemate. Thank you for playing.")
      case color => println(s"${color.name} won! Thank you for playing.")
    }
  }
}
