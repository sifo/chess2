package com.heapseven.chess

import org.scalatest._

class ChessSpec extends FunSpec with Matchers  {
  describe("white pawn") {
    describe("move") {
      it("should be 1 square ahead") {
        val result = {
          val b = """
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
          val m = Move(Position('d', 2), Position('d', 3))
          ChessGame.move(ChessGame(b, White), m)
        }
        val expected = {
          val b = """
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
          ChessGame(b, Black)
        }
        result should be (Right(expected))
      }
      it("should be 2 squares ahead"){
        val result = {
          val b = """
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
          val m = Move(Position('d', 2), Position('d', 4))
          ChessGame.move(ChessGame(b, White), m)
        }
        val expected = {
          val b = """
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
          ChessGame(b, Black)
        }
        result should be (Right(expected))
      }
      it("should not be 3 squares ahead"){
        val result = {
          val b = """
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
          val m = Move(Position('d', 2), Position('d', 5))
          ChessGame.move(ChessGame(b, White), m)
        }
        result should be ('left)
      }
    }
  }
}
