package com.heapseven.chess

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


object Board {
  def toString(b: Board): String = {
    var res = "\n  # Game board\n\n"
    for(i <- b.board.length - 1 to 0 by -1){
      res += "  " + (i + 1) + "  "
      for(j <- 0 to b.board(i).length - 1){
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
        Array(Some(WhiteRook), Some(WhiteKnight), Some(WhiteBishop), Some(WhiteQueen), Some(WhiteKing), Some(WhiteBishop), Some(WhiteKnight), Some(WhiteRook)),
        Array(Some(WhitePawn), Some(WhitePawn), Some(WhitePawn), Some(WhitePawn), Some(WhitePawn), Some(WhitePawn), Some(WhitePawn), Some(WhitePawn)),
        Array(None, None, None, None, None, None, None, None),
        Array(None, None, None, None, None, None, None, None),
        Array(None, None, None, None, None, None, None, None),
        Array(None, None, None, None, None, None, None, None),
        Array(Some(BlackPawn), Some(BlackPawn),Some(BlackPawn),Some(BlackPawn),Some(BlackPawn),Some(BlackPawn),Some(BlackPawn),Some(BlackPawn)),
        Array(Some(BlackRook), Some(BlackKnight), Some(BlackBishop), Some(BlackQueen), Some(BlackKing), Some(BlackBishop), Some(BlackKnight), Some(BlackRook))
      )
    new Board(b)
  }
}
case class Board(val board: Array[Array[Option[Piece]]])

object Chess {
  def main(args: Array[String]) {
    println(Board.toString(Board.newBoard()))
  }
}
