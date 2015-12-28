package com.heapseven.chess

import org.scalatest._

class ChessSpec extends FunSpec with Matchers  {

  describe("Board") {

    describe("when too many pieces") {

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

      it("throws exception"){
        a [IllegalArgumentException] should be thrownBy {
          ChessGame(b, White)
        }
      }
    }

    describe("when pieces are missing") {

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

      it("throws exception"){
        a [IllegalArgumentException] should be thrownBy {
            ChessGame(b, White)
        }
      }
    }
  }

  describe("ChessGame") {

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

    it("rejects position if out of bounds"){
      result should be ('left)
    }
  }

  describe("Promotion") {

    describe("when chessgame status not on promotion pending") {

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
        val c = ChessGame(b, White, Undecided)
        ChessGame.promote(c, WhiteQueen)
      }

      it("fails to promote") {
        result should be ('left)
      }
    }

    describe("when there is no pawn on promote position") {

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
        val c = ChessGame(b, White, PromotionPending(Position('a', 8)))
        ChessGame.promote(c, WhiteQueen)
      }

      it("fails to promote") {
        result should be ('left)
      }
    }

    describe("white pawn") {

      def chessGame = {
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
        val c = ChessGame(b, White, Undecided)
        val m = Move(Position('a', 7), Position('a', 8))
        ChessGame.move(c, m) match {
          case Right(x) => x
          case Left(x) => ChessGame()
        }
      }

      describe("when the pawn is promoted to queen") {

        val result = ChessGame.promote(chessGame, WhiteQueen)

        val expected = {
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

        it("puts the queen on the board"){
          result should be (Right(expected))
        }
      }

      describe("when the pawn is promoted to rook") {

        val result = ChessGame.promote(chessGame, WhiteRook)

        val expected = {
          val b = """
            8   ♖ . . . . . . .
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

        it("puts the rook on the board"){
          result should be (Right(expected))
        }
      }

      describe("when the pawn is promoted to bishop") {

        val result = ChessGame.promote(chessGame, WhiteBishop)

        val expected = {
          val b = """
            8   ♗ . . . . . . .
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

        it("puts the bishop on the board"){
          result should be (Right(expected))
        }
      }

      describe("when the pawn is promoted to Knight") {

        val result = ChessGame.promote(chessGame, WhiteKnight)

        val expected = {
          val b = """
            8   ♘ . . . . . . .
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

        it("puts the knight on the board"){
          result should be (Right(expected))
        }
      }
    }

    describe("black pawn") {

      def chessGame = {
        val b = """
            8   . . . . . . . .
            7   . . . . . ♚ . .
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . . . . . .
            3   . . . . . . . .
            2   ♟ . . . . . . .
            1   . . . . ♔ . . .

                a b c d e f g h
          """
        val c = ChessGame(b, Black, Undecided)
        val m = Move(Position('a', 2), Position('a', 1))
        ChessGame.move(c, m) match {
          case Right(x) => x
          case Left(x) => ChessGame()
        }
      }

      describe("when the pawn is promoted to queen") {

        val result = ChessGame.promote(chessGame, BlackQueen)

        val expected = {
          val b = """
            8   . . . . . . . .
            7   . . . . . ♚ . .
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . . . . . .
            3   . . . . . . . .
            2   . . . . . . . .
            1   ♛ . . . ♔ . . .

                a b c d e f g h
          """
          ChessGame(b, White, Undecided)
        }

        it("puts the queen on the board"){
          result should be (Right(expected))
        }
      }

      describe("when the pawn is promoted to rook") {

        val result = ChessGame.promote(chessGame, BlackRook)

        val expected = {
          val b = """
            8   . . . . . . . .
            7   . . . . . ♚ . .
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . . . . . .
            3   . . . . . . . .
            2   . . . . . . . .
            1   ♜ . . . ♔ . . .

                a b c d e f g h
          """
          ChessGame(b, White, Undecided)
        }

        it("puts the rook on the board"){
          result should be (Right(expected))
        }
      }

      describe("when the pawn is promoted to bishop") {

        val result = ChessGame.promote(chessGame, BlackBishop)

        val expected = {
          val b = """
            8   . . . . . . . .
            7   . . . . . ♚ . .
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . . . . . .
            3   . . . . . . . .
            2   . . . . . . . .
            1   ♝ . . . ♔ . . .

                a b c d e f g h
          """
          ChessGame(b, White, Undecided)
        }

        it("puts the bishop on the board"){
          result should be (Right(expected))
        }
      }

      describe("when the pawn is promoted to knight") {

        val result = ChessGame.promote(chessGame, BlackKnight)

        val expected = {
          val b = """
            8   . . . . . . . .
            7   . . . . . ♚ . .
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . . . . . .
            3   . . . . . . . .
            2   . . . . . . . .
            1   ♞ . . . ♔ . . .

                a b c d e f g h
          """
          ChessGame(b, White, Undecided)
        }

        it("puts the knight on the board"){
          result should be (Right(expected))
        }
      }
    }
  }

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
        ChessGame(b, Black)
      }

      it("puts the pawn 1 square ahead") {
        result should be (Right(expected))
      }
    }

    describe("when move 2 squares ahead"){

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

      it("puts the pawn 2 square ahead"){
        result should be (Right(expected))
      }
    }

    describe("when move 3 squares ahead") {

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

      it("fails to move the pawn"){
        result should be ('left)
      }
    }
  }
}
