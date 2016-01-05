package com.heapseven.chess

import org.scalatest._

class StalemateSpec extends FunSpec with Matchers  {

  describe("Stalemate") {

    describe("when opponent king cannot move 1") {

      val result = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . ♕ . . . ♔ . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   ♚ . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('b', 6), Position('b', 3))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . . . . ♔ . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . ♕ . . . . . .
          2   . . . . . . . .
          1   ♚ . . . . . . .

              a b c d e f g h
        """
        val h = List(Move(Position('b', 6), Position('b', 3)))
        ChessGame(b, Black, Stalemate(Black), h)
      }

      it("provoke stalemate") {
        result should be (Right(expected))
      }
    }

    describe("when opponent king cannot move 2") {

      val result = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . ♚ . . . . .
          3   . . . . . . . .
          2   . . . ♟ . . . .
          1   . . . ♔ . . . .

              a b c d e f g h
        """
        val m = Move(Position('c', 4), Position('d', 3))
        ChessGame.move(ChessGame(b, Black), m)
      }

      val expected = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . ♚ . . . .
          2   . . . ♟ . . . .
          1   . . . ♔ . . . .

              a b c d e f g h
        """
        val h = List(Move(Position('c', 4), Position('d', 3)))
        ChessGame(b, White, Stalemate(White), h)
      }

      it("provoke stalemate") {
        result should be (Right(expected))
      }
    }

    describe("when opponent king cannot move 3") {

      val result = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . ♟ .
          6   . . . . . ♟ . .
          5   . . . . . ♙ . ♔
          4   . . . . ♚ . . ♙
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('e', 4), Position('f', 5))
        ChessGame.move(ChessGame(b, Black), m)
      }

      val expected = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . ♟ .
          6   . . . . . ♟ . .
          5   . . . . . ♚ . ♔
          4   . . . . . . . ♙
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val h = List(Move(Position('e', 4), Position('f', 5)))
        ChessGame(b, White, Stalemate(White), h)
      }

      it("provoke stalemate") {
        result should be (Right(expected))
      }
    }
  }
}
