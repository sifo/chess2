package com.heapseven.chess
import scala.util.matching.Regex

object Piece {
  def toString(p: Option[Piece]): String = {
    p match {
      case None => "."
      case Some(WhitePawn) => "\u2659"
      case Some(WhiteRook) => "\u2656"
      case Some(WhiteQueen) => "\u2655"
      case Some(WhiteKing) => "\u2654"
      case Some(WhiteBishop) => "\u2657"
      case Some(WhiteKnight) => "\u2658"
      case Some(BlackPawn) => "\u265F"
      case Some(BlackRook) => "\u265C"
      case Some(BlackQueen) => "\u265B"
      case Some(BlackKing) => "\u265A"
      case Some(BlackBishop) => "\u265D"
      case Some(BlackKnight) => "\u265E"
    }
  }
}
sealed abstract class Piece
case object WhitePawn extends Piece
case object WhiteRook extends Piece
case object WhiteQueen extends Piece
case object WhiteKing extends Piece
case object WhiteBishop extends Piece
case object WhiteKnight extends Piece
case object BlackPawn extends Piece
case object BlackRook extends Piece
case object BlackQueen extends Piece
case object BlackKing extends Piece
case object BlackBishop extends Piece
case object BlackKnight extends Piece

case object Player {
  def getPlayer(p: Piece): Player = {
    p match {
      case WhitePawn| WhiteRook | WhiteQueen | WhiteKing | WhiteBishop | WhiteKnight => White
      case BlackPawn| BlackRook | BlackQueen | BlackKing | BlackBishop | BlackKnight => Black
    }
  }
}
sealed abstract class Player {
  def opponent: Player
}
case object White extends Player {
  val opponent = Black
}
case object Black extends Player {
  val opponent = White
}

object ChessGame {

  val length = 8

  def apply(b: String, p: Player, s: Status, h: List[Move]) = new ChessGame(b, p, s, h)
  def apply(b: String, p: Player, s: Status) = new ChessGame(b, p, s)
  def apply(b: String, p: Player) = new ChessGame(b, p)
  def apply(b: String) = new ChessGame(b)
  def apply() = new ChessGame()

  def toString(cg: ChessGame): String = {
    var res = "\n"
    for(j <- ChessGame.length - 1 to 0 by -1){
      res += "  " + (j + 1) + "  "
      for(i <- 0 to ChessGame.length - 1){
        res += Piece.toString(cg.board(i)(j)) + " "
      }
      res += "\n"
    }
    res += "\n     a b c d e f g h\n"
    res
  }
  def promote(cg: ChessGame, p: Piece): Either[String, ChessGame] = {
    cg.status match {
      case PromotionPending(dest) =>
      (p, cg.currentPlayer, cg.board(dest.x)(dest.y)) match {
        case (BlackRook | BlackQueen | BlackBishop | BlackKnight, Black, Some(BlackPawn))
           | (WhiteRook | WhiteQueen | WhiteBishop | WhiteKnight, White, Some(WhitePawn)) =>
          val cg2 = ChessGame.replace(cg, Some(p), dest)
          Right(ChessGame(cg2.board, cg2.currentPlayer.opponent, Undecided, cg2.history))
        case (_, _, _) => Left("Invalid action.")
      }
      case _ => Left("Not waiting for promotion.")
    }
  }
  def move(cg: ChessGame, m: Move): Either[String, ChessGame] =  {
    cg.status match {
      case Undecided => ChessGame.validMove(cg, m)
      case DrawRequest => Left("""Draw request pending. Type "nodraw" or "draw".""")
      case Stalemate(p) => Left(s"Stalemate! $p can no longer move.}")
      case Checkmate(p) => Left(s"${p} wins.}")
      case PromotionPending(_) =>
        Left(s"""Promotion pending for ${cg.currentPlayer}. Type "queen", "rook", "bishop" or "knight".""")
    }
  }
  def switchPlayer(p: Player): Player = {
    p match {
      case Black => White
      case White => Black
    }
  }

  def replace(cg: ChessGame, p: Option[Piece], pos: Position): ChessGame = {
    val idx = ChessGame.length * (ChessGame.length-1-pos.y) + pos.x
    val arr = cg._board.updated(idx, p)
    ChessGame(arr, cg.currentPlayer, cg.status, cg.history)
  }

  def _move(cg: ChessGame, p: Piece, m: Move): ChessGame = {
    val cg2 = {
      val c = ChessGame.replace(cg, cg.board(m.src.x)(m.src.y), m.dest)
      ChessGame.replace(c, None, m.src)
    }
    val (player, status) = {
      p match {
        case WhitePawn if m.dest.y == 7 => (cg.currentPlayer, PromotionPending(m.dest))
        case BlackPawn if m.dest.y == 0 => (cg.currentPlayer, PromotionPending(m.dest))
        case _ => (cg.currentPlayer.opponent, Undecided)
      }
    }
    ChessGame(cg2.board, player, status, cg.history :+ m)
  }

  def validMove(cg: ChessGame, m: Move): Either[String, ChessGame] =  {
    if(m.src == m.dest) {
      Left("move in same position")
    } else if (m.src.x > ChessGame.length - 1 || m.src.y > ChessGame.length - 1
                 || m.dest.x > ChessGame.length - 1 || m.dest.y > ChessGame.length - 1) {
      Left("out of bounds move")
    } else {
      (cg.board(m.src.x)(m.src.y), cg.board(m.dest.x)(m.dest.y)) match {
        case (None, _) => Left(s"No piece in ${m.src.x}${m.src.y}.")
        case (Some(p), _) if(cg.currentPlayer != Player.getPlayer(p)) => Left(s"${cg.currentPlayer} turn.")
        case (Some(p), Some(p2)) if (Player.getPlayer(p) == Player.getPlayer(p2)) => Left("can't take your own piece")
        case (Some(p), _) => {
          p match {
            case WhitePawn => validWhitePawnMove(cg, p, m)
            case WhiteRook | BlackRook => validRookMove(cg, p, m)
            case WhiteQueen | BlackQueen => validQueenMove(cg, p, m)
            case WhiteKing | BlackKing => validKingMove(cg, p, m)
            case WhiteBishop | BlackBishop => validBishopMove(cg, p, m)
            case WhiteKnight | BlackKnight => validKnightMove(cg, p, m)
            case BlackPawn => validBlackPawnMove(cg, p, m)
          }
        }
      }
    }
  }

  def validKingMove(cg: ChessGame, p: Piece, m: Move): Either[String, ChessGame] = {
    if ((m.dest.y - m.src.y).abs <= 1 && (m.dest.x - m.src.x).abs <= 1) {
      for(i <- 0 to ChessGame.length - 1)
        for(j <- 0 to ChessGame.length - 1)
          cg.board(i)(j) match {
            case Some(p2) if (Player.getPlayer(p) != Player.getPlayer(p2)) =>
              val cg2 = ChessGame._move(cg, p, m)
              validMove(cg2, Move(Position(i, j), m.dest)) match {
                case Right(x) => return Left(s"Can't put yourself in check")
                case Left(_) =>
              }
            case _ =>
          }
      Right(ChessGame._move(cg, p, m))
    } else {
      Left("Invalid move")
    }
  }

  def validQueenMove(cg: ChessGame, p: Piece, m: Move): Either[String, ChessGame] = {
    validRookMove(cg, p, m) match {
      case Left(_) => validBishopMove(cg, p, m)
      case Right(x) => Right(x)
    }
  }

  def validRookMove(cg: ChessGame, p: Piece, m: Move): Either[String, ChessGame] = {
    def sign(i: Int, j: Int): Int = if(i - j > 0) 1 else -1

    if (m.src.x == m.dest.x ||  m.src.y == m.dest.y) {
      if (m.src.x != m.dest.x) {
        for(i <- 1 to (m.src.x - m.dest.x).abs - 1) {
          cg.board(m.src.x + i * (sign(m.dest.x, m.src.x)))(m.dest.y) match {
            case Some(_) => return Left("Invalid move.")
            case _ =>
          }
        }
      }
      else if (m.src.y != m.dest.y) {
        for(i <- 1 to (m.src.y - m.dest.y).abs - 1) {
          cg.board(m.dest.x)(m.src.y + i * (sign(m.dest.y, m.src.y))) match {
            case Some(_) => return Left("Invalid move.")
            case _ =>
          }
        }
      }
      Right(ChessGame._move(cg, p, m))
    } else Left("Invalid move.")
  }

  def validBishopMove(cg: ChessGame, p: Piece, m: Move): Either[String, ChessGame] = {
    def sign(i: Int, j: Int): Int = if(i - j > 0) 1 else -1

    if ((m.src.x - m.dest.x).abs == (m.src.y - m.dest.y).abs) {
      for(i <- 1 to (m.src.x - m.dest.x).abs - 1) {
        cg.board(m.src.x + i * (sign(m.dest.x, m.src.x)))(m.src.y + i * (sign(m.dest.y, m.src.y))) match {
          case Some(_) => return Left("Invalid move.")
          case _ =>
        }
      }
      Right(ChessGame._move(cg, p, m))
    } else Left("Invalid move.")
  }

  def validKnightMove(cg: ChessGame, p: Piece, m: Move): Either[String, ChessGame] =  {
    val b = (m.src.x - m.dest.x).abs == 1 && (m.src.y - m.dest.y).abs == 2
    val c = (m.src.x - m.dest.x).abs == 2 && (m.src.y - m.dest.y).abs == 1
    if (b || c) {
      Right(ChessGame._move(cg, p, m))
    } else Left("Invalid move.")
  }

  def validWhitePawnMove(cg: ChessGame, p: Piece, m: Move): Either[String, ChessGame] =  {
    if (m.src.y == 1 && m.dest.y == 3 && m.src.x == m.dest.x) {
      (cg.board(m.dest.x)(m.dest.y), cg.board(m.dest.x)(m.dest.y-1)) match {
        case (None, None) => Right(ChessGame._move(cg, p, m))
        case (_, _) => Left("Invalid move")
      }
    } else if (m.dest.y - m.src.y == 1 && m.src.x == m.dest.x) {
      cg.board(m.dest.x)(m.dest.y) match {
        case None => Right(ChessGame._move(cg, p, m))
        case _ => Left("Invalid move")
      }
    } else if (m.dest.y - m.src.y == 1 && (m.src.x - m.dest.x).abs == 1) {
      cg.board(m.dest.x)(m.dest.y) match {
        case Some(p2) => Right(ChessGame._move(cg, p, m))
        case None => {
          cg.board(m.dest.x)(m.dest.y-1) match {
            // look if en passant move
            case Some(p2) if(p2 == BlackPawn
                               && cg.history.length > 0
                               && cg.history.last.dest == Position(m.dest.x, m.dest.y-1)
                               && cg.history.last.src == Position(m.dest.x, m.dest.y+1)) =>
              val cg2 = ChessGame.replace(cg, None, Position(m.dest.x, m.dest.y-1))
              Right(ChessGame._move(cg2, p, m))
            case _ => Left("Invalid move")
          }
        }
      }
    } else {
      Left("Invalid move")
    }
  }

  def validBlackPawnMove(cg: ChessGame, p: Piece, m: Move): Either[String, ChessGame] =  {
    if (m.src.y == 6 && m.dest.y == 4 && m.src.x == m.dest.x) {
      (cg.board(m.dest.x)(m.dest.y), cg.board(m.dest.x)(m.dest.y-1)) match {
        case (None, None) => Right(ChessGame._move(cg, p, m))
        case (_, _) => Left("Invalid move")
      }
    } else if (m.src.y - m.dest.y == 1 && m.src.x == m.dest.x) {
      cg.board(m.dest.x)(m.dest.y) match {
        case None => Right(ChessGame._move(cg, p, m))
        case _ => Left("Invalid move")
      }
    } else if (m.src.y - m.dest.y == 1 && (m.src.x - m.dest.x).abs == 1) {
      cg.board(m.dest.x)(m.dest.y) match {
        case Some(p2) => Right(ChessGame._move(cg, p, m))
        case None => {
          cg.board(m.dest.x)(m.dest.y+1) match {
            // look if en passant move
            case Some(p2) if (p2 == WhitePawn
                                && cg.history.length > 0
                                && cg.history.last.dest == Position(m.dest.x, m.dest.y+1)
                                && cg.history.last.src == Position(m.dest.x, m.dest.y-1)) =>
              val cg2 = ChessGame.replace(cg, None, Position(m.dest.x, m.dest.y+1))
              Right(ChessGame._move(cg2, p, m))
            case _ => Left("Invalid move")
          }
        }
      }
    } else {
      Left("Invalid move")
    }
  }

  def convertChar(s: Char): Option[Piece] = {
    s match {
      case '♖' => Some(WhiteRook)
      case '♘' => Some(WhiteKnight)
      case '♗' => Some(WhiteBishop)
      case '♕' => Some(WhiteQueen)
      case '♔' => Some(WhiteKing)
      case '♙' => Some(WhitePawn)
      case '♜' => Some(BlackRook)
      case '♞' => Some(BlackKnight)
      case '♝' => Some(BlackBishop)
      case '♛' => Some(BlackQueen)
      case '♚' => Some(BlackKing)
      case '♟' => Some(BlackPawn)
      case _ => None
    }
  }
  def convert(s: String): List[Option[Piece]] = {
    val p = List('♖', '♘', '♗', '♕', '♔', '♙', '♜', '♞',  '♝',  '♛',  '♚',  '♟',  '.')
    // val l = {
    val r = s.filter(p.contains(_)).toList.map(convertChar(_))
    if(r.length != 64) 
      throw new IllegalArgumentException("the board should have 64 valid chess characters")
    r
  }
}
case class ChessGame(val _board: List[Option[Piece]], val currentPlayer: Player, val status: Status, val history: List[Move]) {

  def board(): List[Option[Piece]] = _board

  def board(i: Int)(j: Int): Option[Piece] = _board(ChessGame.length * (ChessGame.length-1-j) + i)

  def this(board: String, currentPlayer: Player, status: Status, h: List[Move]) {
    this(ChessGame.convert(board), currentPlayer, status, h)
  }

  def this(board: String, currentPlayer: Player, status: Status) {
    this(ChessGame.convert(board), currentPlayer, status, List[Move]())
  }
  def this(board: String, currentPlayer: Player) {
    this(ChessGame.convert(board), currentPlayer, Undecided, List[Move]())
  }
  def this() {
    this("""
      ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜
      ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟
      . . . . . . . .
      . . . . . . . .
      . . . . . . . .
      . . . . . . . .
      ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙
      ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖
      """, White)
  }
  def this(board: String) {
    this(board, White)
  }
}

object Position {
  def apply(x: Char, y:Int) = new Position(x, y)
}
case class Position(val x: Int, val y: Int) {
  def this(x: Char, y: Int) {
    this(x - 97, y - 1)
  }
}
case class Move(val src: Position, val dest: Position)

sealed abstract class Status
case class Checkmate(player: Player) extends Status
case class Stalemate(player: Player) extends Status
case class PromotionPending(dest: Position) extends Status
case object DrawRequest extends Status
case object Undecided extends Status

object Chess {
  def main(args: Array[String]) {
    var cg = ChessGame()
    var running = true
    while(running) {
      println(ChessGame.toString(cg))
      println(s"${cg.currentPlayer}:")
      print("> ")
      val move = """([a-hA-H])([1-8])[ ]*([a-hA-H])([1-8])""".r
      val line = {
        val scanner = new java.util.Scanner(System.in)
        scanner.nextLine()
      }
      line match {
        case move(x, y, z, t) => {
          ChessGame.move(cg, Move(Position(x.toLowerCase()(0), y.toInt), Position(z.toLowerCase()(0), t.toInt))) match {
            case Right(x) =>
              cg = x
              cg.status match {
                case Stalemate(p) => println(s"Stalemate! $p can no longer move."); running = false
                case Checkmate(p) => println(s"${p} wins."); running = false
                case PromotionPending(d) =>
                  println(s"""Promote pending for ${cg.currentPlayer}. Type "queen", "rook", "bishop" or "knight".""")
                case _ => 
              }
            case Left(x) => println("Error: " + x)
          }
        }
        case "queen" | "rook" | "bishop" | "knight" =>
          val promotion = (line, cg.currentPlayer) match {
            case ("queen", White) => WhiteRook
            case ("rook", White) => WhiteRook
            case ("bishop", White) => WhiteRook
            case ("knight", White) => WhiteRook
            case ("queen", Black) => BlackQueen
            case ("rook", Black) => BlackRook
            case ("bishop", Black) => BlackBishop
            case ("knight", Black) => BlackKnight
            case (_, White) => WhiteQueen
            case (_, Black) => BlackQueen
          }
          cg.status match {
            case PromotionPending(dest) =>
              ChessGame.promote(cg, promotion) match {
                case Right(x) => cg = x
                case Left(x) => println("Error: " + x)
              }
            case _ => println("No promotion pending.")
          }
        case "draw" | "d" =>
          cg.status match {
            case Undecided => cg = ChessGame(cg.board, cg.currentPlayer.opponent, DrawRequest, cg.history)
            case DrawRequest => println("It's draw!"); running = false
            case _ =>
          }
        case "nodraw" | "nd" =>
          cg.status match {
            case Undecided => println("There is no draw request pending.")
            case DrawRequest =>
              cg = ChessGame(cg.board, cg.currentPlayer.opponent, Undecided, cg.history)
              println("Draw request denied!")
            case _ =>
          }
        case "restart" | "r" => cg = ChessGame()
        case "exit" | "e" => running = false
        case "help" | "h" =>
          println("'d' or 'draw' to ask draw to opponent.")
          println("'r' or 'restart' to restart the game.")
          println(""""queen", "rook", "bishop", "knight", to promote.""")
          println("'e' or 'exit' to quit.")
          println("'h' or 'help' for help.")
        case _ => println("Invalid move syntax.")
      }
    }
  }
}
