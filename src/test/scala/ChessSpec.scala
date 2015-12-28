package com.heapseven.chess

import org.scalatest._

class ChessSpec extends FunSpec with Matchers  {
  describe("Board") {
    it("with too many pieces should throw exception"){
      a [IllegalArgumentException] should be thrownBy {
        val result = {
          val b = """
          8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
          7   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
          1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
          val m = Move(Position('d', 2), Position('d', 9))
          ChessGame.move(ChessGame(b, White), m)
        }
      }
    }
    it("with missing pieces should throw exception"){
      a [IllegalArgumentException] should be thrownBy {
        val result = {
          val b = """
          8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
          7   ♟ ♟ ♟ ♟ ♟ ♟
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
          1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
          val m = Move(Position('d', 2), Position('d', 9))
          ChessGame.move(ChessGame(b, White), m)
        }
      }
    }
  }
  describe("Position invalid") {
    it("should be rejected"){
      val result = {
        val b = """
          8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
          7   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
          1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
        val m = Move(Position('d', 2), Position('d', 9))
        ChessGame.move(ChessGame(b, White), m)
      }
      result should be ('left)
    }
  }
  describe("Promotion") {
    it("should failed on a chessgame not waiting for promotion") {
      val result = {
        val b = """
          8   ♙ . . . . . . .
          7   . . . . . ♚ . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . ♔ . . .

              a b c d e f g h
        """
        ChessGame.promote(ChessGame(b, White, Undecided), WhiteQueen)
      }
      result should be ('left)
    }
    it("white pawn"){
      val result = {
        val b = """
          8   . . . . . . . .
          7   ♙ . . . . ♚ . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . ♔ . . .

              a b c d e f g h
        """
        val m = Move(Position('a', 7), Position('a', 8))
        ChessGame.move(ChessGame(b, White, Undecided), m)
      }
      val expected = {
        val b = """
          8   ♙ . . . . . . .
          7   . . . . . ♚ . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . ♔ . . .

              a b c d e f g h
        """
        ChessGame(b, White, PromotionPending(White, Position('a', 8)))
      }
      result should be (Right(expected))
      val result2 = {
        val b = """
          8   ♙ . . . . . . .
          7   . . . . . ♚ . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . ♔ . . .

              a b c d e f g h
        """
        val pos = Position('a', 8)
        ChessGame.promote(ChessGame(b, White, PromotionPending(White, pos)), WhiteQueen)
      }
      val expected2 = {
        val b = """
          8   ♕ . . . . . . .
          7   . . . . . ♚ . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . ♔ . . .

              a b c d e f g h
        """
        ChessGame(b, Black, Undecided)
      }
      result2 should be (Right(expected2))
    }
  }
  describe("white pawn") {
    describe("move") {
      it("should be 1 square ahead") {
        val result = {
          val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . . . . . .
            3   . . . . . . . .
            2   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

                a b c d e f g h
          """
          val m = Move(Position('d', 2), Position('d', 3))
          ChessGame.move(ChessGame(b, White), m)
        }
        val expected = {
          val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . . . . . .
            3   . . . ♙ . . . .
            2   ♙ ♙ ♙ . ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

                a b c d e f g h
          """
          ChessGame(b, Black)
        }
        result should be (Right(expected))
      }
      it("should be 2 squares ahead"){
        val result = {
          val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . . . . . .
            3   . . . . . . . .
            2   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

                a b c d e f g h
          """
          val m = Move(Position('d', 2), Position('d', 4))
          ChessGame.move(ChessGame(b, White), m)
        }
        val expected = {
          val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . ♙ . . . .
            3   . . . . . . . .
            2   ♙ ♙ ♙ . ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

                a b c d e f g h
          """
          ChessGame(b, Black)
        }
        result should be (Right(expected))
      }
      it("should not be 3 squares ahead"){
        val result = {
          val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . . . . . .
            3   . . . . . . . .
            2   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

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
