package com.heapseven.chess

import org.scalatest._

class KnightSpec extends FunSpec with Matchers  {

  describe("knight") {

    describe("when move to valid location") {

      val result = {
        val b = """
          8   . . . . . . . ♚
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . ♘ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('c', 6))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . . . . . ♚
          7   . . . . . . . .
          6   . . ♘ . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val h = List(Move(Position('d', 4), Position('c', 6)))
        ChessGame(b, Black, Undecided, h)
      }

      it("puts the knight in new position") {
        result should be (Right(expected))
      }
    }

    describe("when move to valid location 2") {

      val result = {
        val b = """
          8   . . . . . . ♚ .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . ♘ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . ♔ .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('c', 6))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . . . . ♚ .
          7   . . . . . . . .
          6   . . ♘ . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . ♔ .

              a b c d e f g h
        """
        val h = List(Move(Position('d', 4), Position('c', 6)))
        ChessGame(b, Black, Undecided, h)
      }

      it("puts the knight in new position") {
        result should be (Right(expected))
      }
    }

    describe("when white knight attacks black pawn") {

      val result = {
        val b = """
          8   . . . . . . ♚ .
          7   . . . . . . . .
          6   . . ♟ . . . . .
          5   . . . . . . . .
          4   . . . ♘ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . ♔ .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('c', 6))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . . . . ♚ .
          7   . . . . . . . .
          6   . . ♘ . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . ♔ .

              a b c d e f g h
        """
        val h = List(Move(Position('d', 4), Position('c', 6)))
        ChessGame(b, Black, Undecided, h)
      }

      it("takes the pawn"){
        result should be (Right(expected))
      }
    }

    describe("when black knight attacks white pawn") {

      val result = {
        val b = """
          8   . . . . . . ♚ .
          7   . . . . . . . .
          6   . . ♙ . . . . .
          5   . . . . . . . .
          4   . . . ♞ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . ♔ .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('c', 6))
        ChessGame.move(ChessGame(b, Black), m)
      }

      val expected = {
        val b = """
          8   . . . . . . ♚ .
          7   . . . . . . . .
          6   . . ♞ . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . ♔ .

              a b c d e f g h
        """
        val h = List(Move(Position('d', 4), Position('c', 6)))
        ChessGame(b, White, Undecided, h)
      }

      it("takes the pawn"){
        result should be (Right(expected))
      }
    }

    describe("when it attacks same color piece") {

      val result = {
        val b = """
          8   . . . . . . ♚ .
          7   . . . . . . . .
          6   . . ♙ . . . . .
          5   . . . . . . . .
          4   . . . ♘ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . ♔ .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('c', 6))
        ChessGame.move(ChessGame(b, White), m)
      }

      it("fails to move"){
        result should be ('left)
      }
    }

    describe("when invalid move") {

      val m = List(
        Move(Position('d', 4), Position('d', 5)),
        Move(Position('d', 4), Position('e', 5)),
        Move(Position('d', 4), Position('d', 3)),
        Move(Position('d', 4), Position('a', 4)),
        Move(Position('d', 4), Position('e', 3))
      )

      val cg = {
        val b = """
          8   . . . . . . ♚ .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . ♘ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . ♔ .

              a b c d e f g h
        """
        ChessGame(b, White)
      }

      it("fails to move"){
        m.foreach(ChessGame.move(cg, _) should be ('left))
      }
    }

    describe("when pinned") {

      val result = {
        val b = """
          8   . . . . ♚ . . .
          7   . . . ♞ . . . .
          6   . . . . . . ♟ .
          5   . . . . . . . .
          4   ♗ . . ♙ . . . .
          3   . . . . ♔ . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('d', 7), Position('f', 6))
        ChessGame.move(ChessGame(b, Black), m)
      }

      it("fails to move"){
        result should be ('left)
      }
    }
  }
}
