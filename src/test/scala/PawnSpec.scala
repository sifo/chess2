package com.heapseven.chess

import org.scalatest._

class PawnSpec extends FunSpec with Matchers  {

  describe("White pawn") {

    describe("when move 1 square ahead") {

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
        val h = List(Move(Position('d', 2), Position('d', 3)))
        ChessGame(b, Black, Undecided, h)
      }

      it("puts the pawn 1 square ahead") {
        result should be (Right(expected))
      }
    }

    describe("when move 2 squares ahead"){

      describe("when first move"){
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
          val h = List(Move(Position('d', 2), Position('d', 4)))
          ChessGame(b, Black, Undecided, h)
        }

        it("puts the pawn 2 square ahead"){
          result should be (Right(expected))
        }
      }

      describe("when not first move") {

        val result = {
          val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ ♟ . ♟ ♟
            6   . . . . . . . .
            5   . . . . . ♟ . .
            4   . . . ♙ . . . .
            3   . . . . . . . .
            2   ♙ ♙ ♙ . ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

                a b c d e f g h
          """
          val m = Move(Position('d', 4), Position('d', 6))
          ChessGame.move(ChessGame(b, White), m)
        }

        it("fails to move"){
          result should be ('left)
        }
      }
    }

    describe("when it attacks") {

      val result = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ . . ♟ ♟
            6   . . . . . ♟ . .
            5   . . . . ♟ . . .
            4   . . . ♙ . . . .
            3   . . . . . ♘ . .
            2   ♙ ♙ ♙ . ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
        val m = Move(Position('d', 4), Position('e', 5))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ . . ♟ ♟
            6   . . . . . ♟ . .
            5   . . . . ♙ . . .
            4   . . . . . . . .
            3   . . . . . ♘ . .
            2   ♙ ♙ ♙ . ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
        val h = List(Move(Position('d', 4), Position('e', 5)))
        ChessGame(b, Black, Undecided, h)
      }

      it("takes the pawn"){
        result should be (Right(expected))
      }
    }

    describe("when en passant left") {

      val result = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ . ♜
            7   ♟ . ♟ ♟ ♞ . ♟ ♟
            6   . . . . . ♟ . .
            5   . ♟ ♙ . ♟ . . .
            4   . . . . . . . .
            3   . . . . . ♘ . .
            2   ♙ ♙ . ♙ ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
        val h = List(Move(Position('b', 7), Position('b', 5)))
        val m = Move(Position('c', 5), Position('b', 6))
        ChessGame.move(ChessGame(b, White, Undecided, h), m)
      }

      val expected = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ . ♜
            7   ♟ . ♟ ♟ ♞ . ♟ ♟
            6   . ♙ . . . ♟ . .
            5   . . . . ♟ . . .
            4   . . . . . . . .
            3   . . . . . ♘ . .
            2   ♙ ♙ . ♙ ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
        val h = List(Move(Position('b', 7), Position('b', 5)), Move(Position('c', 5), Position('b', 6)))
        ChessGame(b, Black, Undecided, h)
      }

      it("takes the pawn"){
        result should be (Right(expected))
      }
    }

    describe("when en passant right") {

      val result = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ . ♜
            7   ♟ ♟ ♟ . ♞ . ♟ ♟
            6   . . . . . ♟ . .
            5   . . ♙ ♟ ♟ . . .
            4   . . . . . . . .
            3   . . . . . ♘ . .
            2   ♙ ♙ . ♙ ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
        val h = List(Move(Position('d', 7), Position('d', 5)))
        val m = Move(Position('c', 5), Position('d', 6))
        ChessGame.move(ChessGame(b, White, Undecided, h), m)
      }

      val expected = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ . ♜
            7   ♟ ♟ ♟ . ♞ . ♟ ♟
            6   . . . ♙ . ♟ . .
            5   . . . . ♟ . . .
            4   . . . . . . . .
            3   . . . . . ♘ . .
            2   ♙ ♙ . ♙ ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
          val h = List(Move(Position('d', 7), Position('d', 5)), Move(Position('c', 5), Position('d', 6)))
        ChessGame(b, Black, Undecided, h)
      }

      it("takes the pawn"){
        result should be (Right(expected))
      }
    }

    describe("when invalid move") {

      val m = List(
        Move(Position('d', 4), Position('d', 6)),
        Move(Position('d', 4), Position('e', 5)),
        Move(Position('d', 4), Position('c', 5)),
        Move(Position('d', 4), Position('d', 3)),
        Move(Position('d', 4), Position('e', 3))
      )

      val cg = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ ♟ ♟ . ♟
            6   . . . . . . . .
            5   . . . . . . ♟ .
            4   . . . ♙ . . . .
            3   . . . . . . . .
            2   ♙ ♙ ♙ . ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

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
          7   . . . . . . . .
          6   . ♝ . . . . . .
          5   . . . . . . . .
          4   . . . ♙ . . . .
          3   . . . . ♔ . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('d', 5))
        ChessGame.move(ChessGame(b, White), m)
      }

      it("fails to move"){
        result should be ('left)
      }
    }
  }

  describe("black pawn") {

    describe("when move 1 square ahead") {

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
        val m = Move(Position('d', 7), Position('d', 6))
        ChessGame.move(ChessGame(b, Black), m)
      }

      val expected = {
        val b = """
          8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
          7   ♟ ♟ ♟ . ♟ ♟ ♟ ♟
          6   . . . ♟ . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
          1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

              a b c d e f g h
        """
        val h = List(Move(Position('d', 7), Position('d', 6)))
        ChessGame(b, White, Undecided, h)
      }

      it("puts the pawn 1 square ahead") {
        result should be (Right(expected))
      }
    }

    describe("when move 2 squares ahead"){

      describe("when first move"){
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
          val m = Move(Position('d', 7), Position('d', 5))
          ChessGame.move(ChessGame(b, Black), m)
        }

        val expected = {
          val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ . ♟ ♟ ♟ ♟
            6   . . . . . . . .
            5   . . . ♟ . . . .
            4   . . . . . . . .
            3   . . . . . . . .
            2   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

                a b c d e f g h
          """
          val h = List(Move(Position('d', 7), Position('d', 5)))
          ChessGame(b, White, Undecided, h)
        }

        it("puts the pawn 2 square ahead"){
          result should be (Right(expected))
        }
      }

      describe("when not first move") {

        val result = {
          val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ ♟ . ♟ ♟
            6   . . . . . . . .
            5   . . . . . ♟ . .
            4   . . . . . . . .
            3   . . . . . . . .
            2   ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖

                a b c d e f g h
          """
          val m = Move(Position('f', 5), Position('f', 3))
          ChessGame.move(ChessGame(b, Black), m)
        }

        it("fails to move"){
          result should be ('left)
        }
      }
    }

    describe("when it attacks") {

      val result = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ . ♟ ♟ ♟
            6   . . . . . . . .
            5   . . . . ♟ . . .
            4   . . . ♙ . . . .
            3   . . . . . ♘ . .
            2   ♙ ♙ ♙ . ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
        val m = Move(Position('e', 5), Position('d', 4))
        ChessGame.move(ChessGame(b, Black), m)
      }

      val expected = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ ♟ . ♟ ♟ ♟
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . ♟ . . . .
            3   . . . . . ♘ . .
            2   ♙ ♙ ♙ . ♙ ♙ ♙ ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
        val h = List(Move(Position('e', 5), Position('d', 4)))
        ChessGame(b, White, Undecided, h)
      }

      it("takes the pawn"){
        result should be (Right(expected))
      }
    }

    describe("when en passant left") {

      val result = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ . ♜
            7   ♟ . ♟ ♟ ♞ . ♟ ♟
            6   . . . . . ♟ . .
            5   . . ♙ . ♟ . . .
            4   ♙ ♟ . . . . . .
            3   . . . . . ♘ ♙ .
            2   . ♙ . ♙ ♙ ♙ . ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
        val h = List(Move(Position('a', 2), Position('a', 4)))
        val m = Move(Position('b', 4), Position('a', 3))
        ChessGame.move(ChessGame(b, Black, Undecided, h), m)
      }

      val expected = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ . ♜
            7   ♟ . ♟ ♟ ♞ . ♟ ♟
            6   . . . . . ♟ . .
            5   . . ♙ . ♟ . . .
            4   . . . . . . . .
            3   ♟ . . . . ♘ ♙ .
            2   . ♙ . ♙ ♙ ♙ . ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
        val h = List(Move(Position('a', 2), Position('a', 4)), Move(Position('b', 4), Position('a', 3)))
        ChessGame(b, White, Undecided, h)
      }

      it("takes the pawn"){
        result should be (Right(expected))
      }
    }

    describe("when en passant right") {

      val result = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ . ♜
            7   ♟ . ♟ ♟ ♞ . ♟ ♟
            6   . . . . . ♟ . .
            5   ♙ . . . ♟ . . .
            4   . ♟ ♙ . . . . .
            3   . . . . . ♘ ♙ .
            2   . ♙ . ♙ ♙ ♙ . ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
        val h = List(Move(Position('c', 2), Position('c', 4)))
        val m = Move(Position('b', 4), Position('c', 3))
        ChessGame.move(ChessGame(b, Black, Undecided, h), m)
      }

      val expected = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ . ♜
            7   ♟ . ♟ ♟ ♞ . ♟ ♟
            6   . . . . . ♟ . .
            5   ♙ . . . ♟ . . .
            4   . . . . . . . .
            3   . . ♟ . . ♘ ♙ .
            2   . ♙ . ♙ ♙ ♙ . ♙
            1   ♖ ♘ ♗ ♕ ♔ ♗ . ♖

                a b c d e f g h
          """
          val h = List(Move(Position('c', 2), Position('c', 4)), Move(Position('b', 4), Position('c', 3)))
        ChessGame(b, White, Undecided, h)
      }

      it("takes the pawn"){
        result should be (Right(expected))
      }
    }

    describe("when invalid move") {

      val m = List(
        Move(Position('d', 5), Position('d', 3)),
        Move(Position('d', 5), Position('e', 4)),
        Move(Position('d', 5), Position('c', 4)),
        Move(Position('d', 5), Position('d', 6)),
        Move(Position('d', 5), Position('c', 6))
      )

      val cg = {
        val b = """
            8   ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
            7   ♟ ♟ ♟ . ♟ ♟ ♟ ♟
            6   . . . . . . . .
            5   . . . ♟ . . . .
            4   . . . . . . . .
            3   . . . . . . ♙ .
            2   ♙ ♙ ♙ ♙ ♙ ♙ ♗ ♙
            1   ♖ ♘ ♗ ♕ ♔ . ♘ ♖

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
          7   . . . ♟ . . . .
          6   . . . . . . ♞ .
          5   . . . . . . . .
          4   ♗ . . ♙ . . . .
          3   . . . . ♔ . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('d', 7), Position('d', 6))
        ChessGame.move(ChessGame(b, Black), m)
      }

      it("fails to move"){
        result should be ('left)
      }
    }
  }
}
