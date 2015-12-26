package com.heapseven.chess

import org.scalatest._

class ChessSpec extends FunSpec with Matchers  {
  describe("white pawn") {
    describe("move") {
      it("should be 1 square ahead"){
        val cg = ChessGame.newChessGame()
        val result = ChessGame.move(cg, Move('d', 2, 'd', 3))
        val board: Array[Array[Option[Piece]]] =
          Array(
            Array(Some(WhiteRook), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackRook)),
            Array(Some(WhiteKnight), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKnight)),
            Array(Some(WhiteBishop), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackBishop)),
            Array(Some(WhiteQueen), None, Some(WhitePawn), None, None, None, Some(BlackPawn), Some(BlackQueen)),
            Array(Some(WhiteKing), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKing)),
            Array(Some(WhiteBishop), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackBishop)),
            Array(Some(WhiteKnight), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKnight)),
            Array(Some(WhiteRook), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackRook))
          )
        val expected = ChessGame(board, Black)
        result should be (Right(expected))

      }
      it("should be 2 squares ahead"){
        val cg = ChessGame.newChessGame()
        val result = ChessGame.move(cg, Move('d', 2, 'd', 4))
        val board: Array[Array[Option[Piece]]] =
          Array(
            Array(Some(WhiteRook), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackRook)),
            Array(Some(WhiteKnight), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKnight)),
            Array(Some(WhiteBishop), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackBishop)),
            Array(Some(WhiteQueen), None, None, Some(WhitePawn), None, None, Some(BlackPawn), Some(BlackQueen)),
            Array(Some(WhiteKing), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKing)),
            Array(Some(WhiteBishop), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackBishop)),
            Array(Some(WhiteKnight), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKnight)),
            Array(Some(WhiteRook), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackRook))
          )
        val expected = ChessGame(board, Black)
        result should be (Right(expected))

      }
      it("should not be 3 squares ahead"){
        val cg = ChessGame.newChessGame()
        val result = ChessGame.move(cg, Move('d', 2, 'd', 5))
        val board: Array[Array[Option[Piece]]] =
          Array(
            Array(Some(WhiteRook), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackRook)),
            Array(Some(WhiteKnight), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKnight)),
            Array(Some(WhiteBishop), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackBishop)),
            Array(Some(WhiteQueen), None, None, None, Some(WhitePawn), None, Some(BlackPawn), Some(BlackQueen)),
            Array(Some(WhiteKing), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKing)),
            Array(Some(WhiteBishop), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackBishop)),
            Array(Some(WhiteKnight), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackKnight)),
            Array(Some(WhiteRook), Some(WhitePawn), None, None, None, None, Some(BlackPawn), Some(BlackRook))
          )
        val expected = ChessGame(board, Black)
        result should not be (Right(expected))

      }
    }
  }
}
