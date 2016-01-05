package com.heapseven.chess

import org.scalatest._

class CheckmateSpec extends FunSpec with Matchers  {

  describe("Checkmate") {

    describe("when checkmate 1") {

      val result = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . . . . ♔ . ♚
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . ♕ . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('b', 2), Position('h', 2))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . . . . ♔ . ♚
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . ♕
          1   . . . . . . . .

              a b c d e f g h
        """
        val h = List(Move(Position('b', 2), Position('h', 2)))
        ChessGame(b, Black, Checkmate(White), h)
      }

      it("sets game as checkmate") {
        result should be (Right(expected))
      }
    }

    describe("when checkmate 2") {

      val result = {
        val b = """
          8   . . . . . . ♚ .
          7   . . . . . ♟ ♟ ♟
          6   . . ♖ . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . ♔ . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('c', 6), Position('c', 8))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . ♖ . . . ♚ .
          7   . . . . . ♟ ♟ ♟
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . ♔ . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val h = List(Move(Position('c', 6), Position('c', 8)))
        ChessGame(b, Black, Checkmate(White), h)
      }

      it("sets game as checkmate") {
        result should be (Right(expected))
      }
    }

    describe("when checkmate 3") {

      val result = {
        val b = """
          8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
          7   ♟ ♟ ♟ ♟ . ♟ ♟ ♟
          6   . . . . . . . .
          5   . . . . ♟ . . .
          4   . . . . . . ♙ .
          3   . . . . . ♙ . .
          2   ♙ ♙ ♙ ♙ ♙ . . ♙
          1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
        val m = Move(Position('d', 8), Position('h', 4))
        ChessGame.move(ChessGame(b, Black), m)
      }

      val expected = {
        val b = """
          8   ♜ ♞ ♝ . ♚ ♝ ♞ ♜
          7   ♟ ♟ ♟ ♟ . ♟ ♟ ♟
          6   . . . . . . . .
          5   . . . . ♟ . . .
          4   . . . . . . ♙ ♛
          3   . . . . . ♙ . .
          2   ♙ ♙ ♙ ♙ ♙ . . ♙
          1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
        val h = List(Move(Position('d', 8), Position('h', 4)))
        ChessGame(b, White, Checkmate(Black), h)
      }

      it("sets game as checkmate") {
        result should be (Right(expected))
      }
    }
  }
}
