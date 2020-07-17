package main

import agent._
import board._
import helper._
import helper.Color._

object PawnRace {
  def filterPlayer(
    arg: String, game: Game, board: Board, color: Color
  ): Option[AbstractPlayer] = arg match {
    case "P" => Some(new HumanPlayer(game, board, color))
    case "C" => Some(new ComputerPlayer(game, board, color))
    case _ => None
  }

  def filterGap(arg: String): Option[Int] = if (arg.length == 1)
    Some(arg(0).toUpper).map(_ - 'A').filter(i => 0 <= i && i < 8)
  else
    None

  def main(args: Array[String]): Unit = {
    if (args.length != 4) {
      Console.err.println("Usage: [P/C](White) [P/C](Black) [A-H](White gap) [A-H](Black gap)")
      sys.exit(1)
    }

    /** Parse and init board */
    val board = (for {
      whiteGap <- filterGap(args(2))
      blackGap <- filterGap(args(3))
    } yield new Board(whiteGap, blackGap)).getOrElse({
      Console.err.println("Invalid white gap and/or black gap")
      sys.exit(2)
    })

    /** Init game with board */
    val game = new Game(board)

    /** Init players */
    val players = (for {
      whitePlayer <- filterPlayer(args(0), game, board, WHITE)
      blackPlayer <- filterPlayer(args(1), game, board, BLACK)
    } yield {
      whitePlayer.opponent = blackPlayer
      blackPlayer.opponent = whitePlayer
      Map[Color, AbstractPlayer](WHITE -> whitePlayer, BLACK -> blackPlayer)
    }).getOrElse({
      Console.err.println("Invalid white player and/or black player")
      sys.exit(3)
    })

    /** actually play the game */
    while (!game.isFinished) {
      board.display()
      players(game.currentPlayer).makeMove()
    }

    /** display result */
    board.display()
    println(s"${game.gameResult.name} won!")
  }
}
