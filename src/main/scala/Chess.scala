package com.heapseven.chess
import scala.util.matching.Regex

object Piece {
  def toString(p: Option[Piece]): String = {
    p match {
      case None => "."
      case Some(WhitePawn) => "\u2659"
      case Some(WhiteRook) => "\u2656"
      case Some(WhiteQueen) => "\u2655"
      case Some(WhiteKing) => "\u2654"
      case Some(WhiteBishop) => "\u2657"
      case Some(WhiteKnight) => "\u2658"
      case Some(BlackPawn) => "\u265F"
      case Some(BlackRook) => "\u265C"
      case Some(BlackQueen) => "\u265B"
      case Some(BlackKing) => "\u265A"
      case Some(BlackBishop) => "\u265D"
      case Some(BlackKnight) => "\u265E"
    }
  }
}
sealed abstract class Piece
case object WhitePawn extends Piece
case object WhiteRook extends Piece
case object WhiteQueen extends Piece
case object WhiteKing extends Piece
case object WhiteBishop extends Piece
case object WhiteKnight extends Piece
case object BlackPawn extends Piece
case object BlackRook extends Piece
case object BlackQueen extends Piece
case object BlackKing extends Piece
case object BlackBishop extends Piece
case object BlackKnight extends Piece

case object Player {
  def getPlayer(p: Piece): Player = {
    p match {
      case WhitePawn| WhiteRook | WhiteQueen | WhiteKing | WhiteBishop | WhiteKnight => White
      case BlackPawn| BlackRook | BlackQueen | BlackKing | BlackBishop | BlackKnight => Black
    }
  }
}
sealed abstract class Player
case object White extends Player
case object Black extends Player

object ChessGame {

  def toString(cg: ChessGame): String = {
    var res = "\n  # Game board\n\n"
    for(j <- cg.board(0).length - 1 to 0 by -1){
      res += "  " + (j + 1) + "  "
      for(i <- 0 to cg.board.length - 1){
        res += Piece.toString(cg.board(i)(j)) + " "
      }
      res += "\n"
    }
    res += "\n     a b c d e f g h\n"
    res
  }
  def newChessGame(): ChessGame = {
    val cg: Array[Array[Option[Piece]]] =
      Array(
        Array(Some(WhiteRook), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackRook)),
        Array(Some(WhiteKnight), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKnight)),
        Array(Some(WhiteBishop), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackBishop)),
        Array(Some(WhiteQueen), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackQueen)),
        Array(Some(WhiteKing), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKing)),
        Array(Some(WhiteBishop), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackBishop)),
        Array(Some(WhiteKnight), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKnight)),
        Array(Some(WhiteRook), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackRook))
      )
    new ChessGame(cg, White)
  }
  def move(cg: ChessGame, m: Move): Either[String, ChessGame] =  {
    ChessGame.getPiece(cg, m.x, m.y) match {
      case None => Left(s"No piece in ${m.x}${m.y}.")
      case Some(p) => {
        (cg.currentPlayer, Player.getPlayer(p)) match {
          case (White, White) | (Black, Black) => {
            if(ChessGame.validMove(cg, p, m)) {
              Right(ChessGame._move(cg, p, m))
            } else {
              Left("Invalid move.")
            }
          }
          case (White, Black) | (Black, White) => Left(s"${cg.currentPlayer} turn.")
        }
      }
    }
  }
  def switchPlayer(p: Player): Player =  {
    p match {
      case Black => White
      case White => Black
    }
  }
  def getPiece(cg: ChessGame, x: Char, y: Int): Option[Piece] = {
    cg.board(Move.alphaIndex(x))(y - 1)
  }
  def _move(cg: ChessGame, p: Piece, m: Move): ChessGame = {
    p match {
      case WhitePawn if m.t == 8 => cg.board(Move.alphaIndex(m.z))(m.t - 1) = Some(WhiteQueen)
      case BlackPawn if m.t == 1 => cg.board(Move.alphaIndex(m.z))(m.t - 1) = Some(BlackQueen)
      case _ => cg.board(Move.alphaIndex(m.z))(m.t - 1) = cg.board(Move.alphaIndex(m.x))(m.y - 1)
    }
    cg.board(Move.alphaIndex(m.x))(m.y - 1) = None
    new ChessGame(cg.board, ChessGame.switchPlayer(cg.currentPlayer))
  }
  def validMove(cg: ChessGame, p: Piece, m: Move): Boolean =  {
    if(m.x == m.z && m.y == m.t) {
      false
    } else {
      p match {
        case WhitePawn => validWhitePawnMove(cg, p, m)
        case WhiteRook => true
        case WhiteQueen | BlackQueen => validQueenMove(cg, p, m)
        case WhiteKing => true
        case WhiteBishop => true
        case WhiteKnight => true
        case BlackPawn => validBlackPawnMove(cg, p, m)
        case BlackRook => true
        case BlackKing => true
        case BlackBishop => true
        case BlackKnight => true
      }

    }
  }
  def validWhitePawnMove(cg: ChessGame, p: Piece, m: Move): Boolean =  {
    if (m.y == 2 && m.t == 4 && m.x == m.z) {
      (ChessGame.getPiece(cg, m.z, m.t), ChessGame.getPiece(cg, m.z, m.t-1)) match {
        case (None, None) => true
        case (_, _) => false
      }
    } else if (m.t - m.y == 1 && m.x == m.z) {
      ChessGame.getPiece(cg, m.z, m.t) match {
        case None => true
        case _ => false
      }
    } else if (m.t - m.y == 1 && (Move.alphaIndex(m.x) - Move.alphaIndex(m.z)).abs == 1) {
      ChessGame.getPiece(cg, m.z, m.t) match {
        case Some(p2) => {
          (Player.getPlayer(p), Player.getPlayer(p2)) match {
            case (White, White) => false
            case _ => true
          }
        }
        case None => false
      }
    } else {
      false
    }
  }
  def validQueenMove(cg: ChessGame, p: Piece, m: Move): Boolean =  {
    if ((m.t - m.y).abs <= 1 && (Move.alphaIndex(m.x) - Move.alphaIndex(m.z)).abs <= 1) {
      ChessGame.getPiece(cg, m.z, m.t) match {
        case Some(p2) => {
          (Player.getPlayer(p), Player.getPlayer(p2)) match {
            case (White, White) | (Black, Black) => false
            case _ => true
          }
        }
        case None => true
      }
    } else {
      false
    }
  }
  def validBlackPawnMove(cg: ChessGame, p: Piece, m: Move): Boolean =  {
    if (m.y == 7 && m.t == 5 && m.x == m.z) {
      (ChessGame.getPiece(cg, m.z, m.t), ChessGame.getPiece(cg, m.z, m.t+1)) match {
        case (None, None) => true
        case (_, _) => false
      }
    } else if (m.y - m.t == 1 && m.x == m.z) {
      ChessGame.getPiece(cg, m.z, m.t) match {
        case None => true
        case _ => false
      }
    } else if (m.y - m.t == 1 && (Move.alphaIndex(m.x) - Move.alphaIndex(m.z)).abs == 1) {
      ChessGame.getPiece(cg, m.z, m.t) match {
        case Some(p2) => {
          (Player.getPlayer(p), Player.getPlayer(p2)) match {
            case (Black, Black) => false
            case _ => true
          }
        }
        case None => false
      }
    } else {
      false
    }

  }
}
case class ChessGame(val board: Array[Array[Option[Piece]]], val currentPlayer: Player)

object Move {
  def alphaIndex(c: Char): Int = {
    val tmp = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
    for(i <- 0 to tmp.length - 1){
      if(c == tmp(i))
        return i
    }
    -1
  }
}
case class Move(val x: Char, val y: Int, val z: Char, val t: Int)

object Chess {
  def main(args: Array[String]) {
    var cg = ChessGame.newChessGame()
    while(true) {
      println(ChessGame.toString(cg))
      print("> ")
      val move = """([a-hA-H])([1-8])[ ]*([a-hA-H])([1-8])""".r
      val scanner = new java.util.Scanner(System.in)
      val line = scanner.nextLine()
      line match {
        case move(x, y, z, t) => {
          ChessGame.move(cg, new Move(x.toLowerCase()(0), y.toInt, z.toLowerCase()(0), t.toInt)) match {
            case Right(x) => cg = x
            case Left(x) => println(x)
          }
        }
        case _ => println("Invalid move syntax.")
      }
    }
  }
}
