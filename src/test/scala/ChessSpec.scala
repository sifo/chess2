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
        a [IllegalArgumentException] should be thrownBy { ChessGame(b, White) }
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
        a [IllegalArgumentException] should be thrownBy { ChessGame(b, White) }
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
        ChessGame.move(c, m) match { case Right(x) => x; case Left(x) => ChessGame() }
      }

      describe("when promoted to valid pieces") {

        val p = List((WhiteQueen, '♕'), (WhiteRook, '♖'), (WhiteBishop, '♗'), (WhiteKnight, '♘'))

        def expected(c: Char) = {
          val b = s"""
            8  $c . . . . . . .
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

        it("puts the piece on the board") {
          p.foreach { t => ChessGame.promote(chessGame, t._1) should be (Right(expected(t._2))) }
        }
      }

      describe("when promoted to invalid pieces") {

        val p = List(WhitePawn, WhiteKing, BlackPawn, BlackRook, BlackQueen, BlackKing, BlackBishop, BlackKnight)

        it("fails to put the pieces on the board"){
          p.foreach(ChessGame.promote(chessGame, _) should be ('left))
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
        ChessGame.move(c, m) match { case Right(x) => x; case Left(x) => ChessGame() }
      }

      describe("when promoted to valid pieces") {

        val p = List((BlackQueen, '♛'), (BlackRook, '♜'), (BlackBishop, '♝'), (BlackKnight, '♞'))

        def expected(c: Char) = {
          val b = s"""
            8   . . . . . . . .
            7   . . . . . ♚ . .
            6   . . . . . . . .
            5   . . . . . . . .
            4   . . . . . . . .
            3   . . . . . . . .
            2   . . . . . . . .
            1  $c . . . ♔ . . .

                a  b c d e f g h
          """
          ChessGame(b, White, Undecided)
        }

        it("puts the piece on the board") {
          p.foreach { t => ChessGame.promote(chessGame, t._1) should be (Right(expected(t._2))) }
        }
      }

      describe("when promoted to invalid pieces") {

        val p = List(BlackPawn, BlackKing, WhitePawn, WhiteRook, WhiteQueen, WhiteKing, WhiteBishop, WhiteKnight)

        it("fails to put the pieces on the board"){
          p.foreach(ChessGame.promote(chessGame, _) should be ('left))
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
          ChessGame(b, Black)
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
        ChessGame(b, Black)
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
        val m = Move(Position('c', 5), Position('b', 6))
        println(s"${Position('b', 6)}")
        val h = List(Move(Position('b', 7), Position('b', 5)))
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
        val m = Move(Position('c', 5), Position('d', 6))
        val h = List(Move(Position('d', 7), Position('d', 5)))
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
  }
}
