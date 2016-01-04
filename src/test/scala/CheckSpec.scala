package com.heapseven.chess

import org.scalatest._

class CheckSpec extends FunSpec with Matchers  {

  describe("Check") {

    describe("when king move into check if the attacking piece is pinned") {

      val result = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   ♖ . . . ♟ ♚ . .
          5   . . . . . . . .
          4   . . ♔ . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('c', 4), Position('d', 5))
        ChessGame.move(ChessGame(b, White), m)
      }

      it("fails to puts in new position") {
        result should be ('left)
      }
    }

    describe("when pinned rook try to check opponent's king") {

      val result = {
        val b = """
          8   . . . . ♚ . . .
          7   . . . ♜ . . . .
          6   . . . . . . ♟ .
          5   . . . . . . . .
          4   ♗ . . ♙ ♔ . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('d', 7), Position('e', 7))
        ChessGame.move(ChessGame(b, Black), m)
      }

      it("fails to puts in new position") {
        result should be ('left)
      }
    }
  }
}
