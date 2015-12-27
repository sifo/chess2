package com.heapseven.chess

import org.scalatest._

class ChessSpec extends FunSpec with Matchers  {
  describe("white pawn") {
    describe("move") {
      it("should be 1 square ahead") {
        val board = """
          1   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
          2   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
          3   . . . . . . . .
          4   . . . . . . . .
          5   . . . . . . . .
          6   . . . . . . . .
          7   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
          8   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
        val cg = ChessGame(board, White)
        val result = ChessGame.move(cg, Move('d', 2, 'd', 3))
        val expectedBoard = """
          1   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
          2   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
          3   . . . . . . . .
          4   . . . . . . . .
          5   . . . . . . . .
          6   . . . ♙ . . . .
          7   ♙ ♙ ♙ . ♙ ♙ ♙ ♙
          8   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
        val expectedChessGame = ChessGame(expectedBoard, Black)
        result should be (Right(expectedChessGame))
      }
      it("should be 2 squares ahead"){
        val board = """
          1   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
          2   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
          3   . . . . . . . .
          4   . . . . . . . .
          5   . . . . . . . .
          6   . . . . . . . .
          7   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
          8   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
        val cg = ChessGame(board, White)
        val result = ChessGame.move(cg, Move('d', 2, 'd', 4))
        val expectedBoard = """
          1   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
          2   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
          3   . . . . . . . .
          4   . . . . . . . .
          5   . . . ♙ . . . .
          6   . . . . . . . .
          7   ♙ ♙ ♙ . ♙ ♙ ♙ ♙
          8   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
        val expectedChessGame = ChessGame(expectedBoard, Black)
        result should be (Right(expectedChessGame))
      }
      it("should not be 3 squares ahead"){
        val board = """
          1   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
          2   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
          3   . . . . . . . .
          4   . . . . . . . .
          5   . . . . . . . .
          6   . . . . . . . .
          7   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
          8   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
        val cg = ChessGame(board, White)
        val result = ChessGame.move(cg, Move('d', 2, 'd', 5))
        val expectedBoard = """
          1   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
          2   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
          3   . . . . . . . .
          4   . . . ♙ . . . .
          5   . . . . . . . .
          6   . . . . . . . .
          7   ♙ ♙ ♙ . ♙ ♙ ♙ ♙
          8   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
        val expectedChessGame = ChessGame(expectedBoard, White)
        result should be ('left)
      }
    }
  }
}
