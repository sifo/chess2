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

object Board {
  var currentPlayer: Player = White

  def toString(b: Board): String = {
    var res = "\n  # Game board\n\n"
    for(j <- b.board(0).length - 1 to 0 by -1){
      res += "  " + (j + 1) + "  "
      for(i <- 0 to b.board.length - 1){
        res += Piece.toString(b.board(i)(j)) + " "
      }
      res += "\n"
    }
    res += "\n     a b c d e f g h\n"
    res
  }
  def newBoard(): Board = {
    val b: Array[Array[Option[Piece]]] =
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
    new Board(b)
  }
  def move(b: Board, m: Move): Option[String] =  {
    Board.getPiece(b, m.x, m.y) match {
      case None => Some(s"No piece in ${m.x}${m.y}.")
      case Some(p) => {
        (currentPlayer, Player.getPlayer(p)) match {
          case (White, White) | (Black, Black) => {
            if(Board.validMove(b, p, m)) {
              Board._move(b, p, m);
              currentPlayer = Board.switchPlayer(currentPlayer);
              None
            } else {
              Some("Invalid move.")
            }
          }
          case (White, Black) | (Black, White) => Some(s"$currentPlayer turn.")
        }
      }
    }
  }
  def switchPlayer(p: Player): Player =  {
    currentPlayer match {
      case Black => White
      case White => Black
    }
  }
  def getPiece(b: Board, x: Char, y: Int): Option[Piece] = {
    b.board(Move.alphaIndex(x))(y - 1)
  }
  def _move(b: Board, p: Piece, m: Move) {
    p match {
      case WhitePawn if m.t == 8 => 
        b.board(Move.alphaIndex(m.z))(m.t - 1) = Some(WhiteQueen)
      case BlackPawn if m.t == 1 => 
        b.board(Move.alphaIndex(m.z))(m.t - 1) = Some(BlackQueen)
      case _ => 
        b.board(Move.alphaIndex(m.z))(m.t - 1) = b.board(Move.alphaIndex(m.x))(m.y - 1)
    }
    b.board(Move.alphaIndex(m.x))(m.y - 1) = None
  }
  def validMove(b: Board, p: Piece, m: Move): Boolean =  {
    p match {
      case WhitePawn => {
        if (m.y == 2 && m.t == 4 && m.x == m.z) {
          (Board.getPiece(b, m.z, m.t), Board.getPiece(b, m.z, m.t-1)) match {
            case (None, None) => true
            case (_, _) => false
          }
        } else if (m.t - m.y == 1 && m.x == m.z) {
          Board.getPiece(b, m.z, m.t) match {
            case None => true
            case _ => false
          }
        } else if (m.t - m.y == 1 && (Move.alphaIndex(m.x) - Move.alphaIndex(m.z)).abs == 1) {
          Board.getPiece(b, m.z, m.t) match {
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
      case WhiteRook => true
      case WhiteQueen => true
      case WhiteKing => true
      case WhiteBishop => true
      case WhiteKnight => true
      case BlackPawn => true
      case BlackRook => true
      case BlackQueen => true
      case BlackKing => true
      case BlackBishop => true
      case BlackKnight => true
    }
  }
}
case class Board(val board: Array[Array[Option[Piece]]])

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
    val b = Board.newBoard()
    while(true) {
      println(Board.toString(b))
      print("> ")
      val move = """([a-hA-H])([1-8])[ ]*([a-hA-H])([1-8])""".r
      val scanner = new java.util.Scanner(System.in)
      val line = scanner.nextLine()
      line match {
        case move(x, y, z, t) => {
          Board.move(b, new Move(x.toLowerCase()(0), y.toInt, z.toLowerCase()(0), t.toInt)) match {
            case None => 
            case Some(s) => println(s)
          }
        }
        case _ => println("Invalid move syntax.")
      }
    }
  }
}
