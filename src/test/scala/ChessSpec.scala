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
          val h = List(Move(Position('a', 7), Position('a', 8)))
          ChessGame(b, Black, Undecided, h)
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
          val h = List(Move(Position('a', 2), Position('a', 1)))
          ChessGame(b, White, Undecided, h)
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
  }

  describe("knight") {

    describe("when move to valid location") {

      val result = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . ♘ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('c', 6))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . ♘ . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

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
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . ♘ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('c', 6))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . ♘ . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

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
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . ♟ . . . . .
          5   . . . . . . . .
          4   . . . ♘ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('c', 6))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . ♘ . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

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
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . ♙ . . . . .
          5   . . . . . . . .
          4   . . . ♞ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('c', 6))
        ChessGame.move(ChessGame(b, Black), m)
      }

      val expected = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . ♞ . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

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
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . ♙ . . . . .
          5   . . . . . . . .
          4   . . . ♘ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

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
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . ♘ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

              a b c d e f g h
        """
        ChessGame(b, White)
      }

      it("fails to move"){
        m.foreach(ChessGame.move(cg, _) should be ('left))
      }
    }
  }

  describe("bishop") {

    describe("when move to valid location") {

      val result = {
        val b = """
          8   . . . ♚ . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . ♗ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('a', 7))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . ♚ . . . .
          7   ♗ . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val h = List(Move(Position('d', 4), Position('a', 7)))
        ChessGame(b, Black, Undecided, h)
      }

      it("puts the bishop in new position") {
        result should be (Right(expected))
      }
    }

    describe("when move to valid location 2") {

      val result = {
        val b = """
          8   . . . ♚ . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . ♗ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('f', 2))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . ♚ . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . ♗ . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val h = List(Move(Position('d', 4), Position('f', 2)))
        ChessGame(b, Black, Undecided, h)
      }

      it("puts the bishop in new position") {
        result should be (Right(expected))
      }
    }

    describe("when move over one piece") {

      val result = {
        val b = """
          8   . . . ♚ . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . ♘ . . . . .
          4   . . . ♗ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('a', 7))
        ChessGame.move(ChessGame(b, White), m)
      }

      it("fails to move bishop over one piece") {
        result should be ('left)
      }
    }

    describe("when move over one piece 2") {

      val result = {
        val b = """
          8   . . . ♚ . . . .
          7   . . . . . . ♙ .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . ♗ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('h', 8))
        ChessGame.move(ChessGame(b, White), m)
      }

      it("fails to move bishop over one piece") {
        result should be ('left)
      }
    }

    describe("when white bishop attacks black pawn") {

      val result = {
        val b = """
          8   . . . ♚ . . . .
          7   . . . . . . . .
          6   . ♟ . . . . . .
          5   . . . . . . . .
          4   . . . ♗ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('b', 6))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . ♚ . . . .
          7   . . . . . . . .
          6   . ♗ . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val h = List(Move(Position('d', 4), Position('b', 6)))
        ChessGame(b, Black, Undecided, h)
      }

      it("takes the pawn"){
        result should be (Right(expected))
      }
    }

    describe("when white bishop attacks black pawn 2") {

      val result = {
        val b = """
          8   . . . ♚ . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . ♟ . . . . .
          4   . . . ♗ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('c', 5))
        ChessGame.move(ChessGame(b, White), m)
      }

      val expected = {
        val b = """
          8   . . . ♚ . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . ♗ . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val h = List(Move(Position('d', 4), Position('c', 5)))
        ChessGame(b, Black, Undecided, h)
      }

      it("takes the pawn"){
        result should be (Right(expected))
      }
    }

    describe("when black bishop attacks white pawn") {

      val result = {
        val b = """
          8   . . . ♚ . . . .
          7   . . . . . . . .
          6   . ♙ . . . . . .
          5   . . . . . . . .
          4   . . . ♝ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('b', 6))
        ChessGame.move(ChessGame(b, Black), m)
      }

      val expected = {
        val b = """
          8   . . . ♚ . . . .
          7   . . . . . . . .
          6   . ♝ . . . . . .
          5   . . . . . . . .
          4   . . . . . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . ♔ . .

              a b c d e f g h
        """
        val h = List(Move(Position('d', 4), Position('b', 6)))
        ChessGame(b, White, Undecided, h)
      }

      it("takes the pawn"){
        result should be (Right(expected))
      }
    }

    describe("when white attacks same color piece") {

      val result = {
        val b = """
          8   . . . . . . ♚ .
          7   . . . . . . . .
          6   . ♙ . . . . . .
          5   . . . . . . . .
          4   . . . ♗ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . ♔ .

              a b c d e f g h
        """
        val m = Move(Position('d', 4), Position('b', 6))
        ChessGame.move(ChessGame(b, White), m)
      }

      it("fails to move"){
        result should be ('left)
      }

      describe("when black attacks same color piece") {

        val result = {
          val b = """
          8   . . . . . . ♚ .
          7   . . . . . . . .
          6   . ♟ . . . . . .
          5   . . . . . . . .
          4   . . . ♝ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . ♔ .

              a b c d e f g h
        """
          val m = Move(Position('d', 4), Position('b', 6))
          ChessGame.move(ChessGame(b, Black), m)
        }

        it("fails to move"){
          result should be ('left)
        }
      }
    }

    describe("when invalid move") {

      val m = List(
        Move(Position('d', 4), Position('d', 5)),
        Move(Position('d', 4), Position('e', 6)),
        Move(Position('d', 4), Position('d', 3)),
        Move(Position('d', 4), Position('c', 2)),
        Move(Position('d', 4), Position('a', 4)),
        Move(Position('d', 4), Position('f', 4))
      )

      val cg = {
        val b = """
          8   . . . . . . . .
          7   . . . . . . . .
          6   . . . . . . . .
          5   . . . . . . . .
          4   . . . ♗ . . . .
          3   . . . . . . . .
          2   . . . . . . . .
          1   . . . . . . . .

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
