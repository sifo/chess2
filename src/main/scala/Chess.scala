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

  def apply(b: String, p: Player) = new ChessGame(b, p)
  def apply(b: String) = new ChessGame(b)
  def apply() = new ChessGame()

  def toString(cg: ChessGame): String = {
    var res = "\n"
    for(j <- cg.board(0).length - 1 to 0 by -1){
      res += "  " + (j + 1) + "  "
      for(i <- 0 to cg.board.length - 1){
        res += Piece.toString(cg.board(i)(j)) + " "
      }
      res += "\n"
    }
    res += "\n     a b c d e f g h\n"
    res
  }
  def move(cg: ChessGame, m: Move): Either[String, ChessGame] =  {
    if(m.src.x > cg.board.length - 1 || m.src.y > cg.board(m.src.x).length - 1)
      Left("Invalid move")
    else cg.board(m.src.x)(m.src.y) match {
      case None => Left(s"No piece in ${m.src.x}${m.src.y}.")
      case Some(p) => {
        (cg.currentPlayer, Player.getPlayer(p)) match {
          case (White, White) | (Black, Black) => {
            if(ChessGame.validMove(cg, p, m)) {
              Right(ChessGame._move(cg, p, m))
            } else {
              Left("Invalid move.")
            }
          }
          case (White, Black) | (Black, White) => Left(s"${cg.currentPlayer} turn.")
        }
      }
    }
  }
  def switchPlayer(p: Player): Player = {
    p match {
      case Black => White
      case White => Black
    }
  }
  def _move(cg: ChessGame, p: Piece, m: Move): ChessGame = {
    p match {
      case WhitePawn if m.dest.y == 7 => cg.board(m.dest.x)(m.dest.y) = Some(WhiteQueen)
      case BlackPawn if m.dest.y == 0 => cg.board(m.dest.x)(m.dest.y) = Some(BlackQueen)
      case _ => cg.board(m.dest.x)(m.dest.y) = cg.board(m.src.x)(m.src.y)
    }
    cg.board(m.src.x)(m.src.y) = None
    ChessGame(cg.board, ChessGame.switchPlayer(cg.currentPlayer), Undecided)
  }
  def validMove(cg: ChessGame, p: Piece, m: Move): Boolean =  {
    if(m.src == m.dest) {
      false
    } else {
      p match {
        case WhitePawn => validWhitePawnMove(cg, p, m)
        case WhiteRook => true
        case WhiteQueen | BlackQueen => validQueenMove(cg, p, m)
        case WhiteKing => true
        case WhiteBishop => true
        case WhiteKnight => true
        case BlackPawn => validBlackPawnMove(cg, p, m)
        case BlackRook => true
        case BlackKing => true
        case BlackBishop => true
        case BlackKnight => true
      }

    }
  }
  def validWhitePawnMove(cg: ChessGame, p: Piece, m: Move): Boolean =  {
    if (m.src.y == 1 && m.dest.y == 3 && m.src.x == m.dest.x) {
      (cg.board(m.dest.x)(m.dest.y), cg.board(m.dest.x)(m.dest.y-1)) match {
        case (None, None) => true
        case (_, _) => false
      }
    } else if (m.dest.y - m.src.y == 1 && m.src.x == m.dest.x) {
      cg.board(m.dest.x)(m.dest.y) match {
        case None => true
        case _ => false
      }
    } else if (m.dest.x - m.src.y == 1 && (m.src.x - m.dest.x).abs == 1) {
      cg.board(m.dest.x)(m.dest.y) match {
        case Some(p2) => {
          (Player.getPlayer(p), Player.getPlayer(p2)) match {
            case (White, White) => false
            case _ => true
          }
        }
        case None => false
      }
    } else {
      false
    }
  }
  def validQueenMove(cg: ChessGame, p: Piece, m: Move): Boolean =  {
    if ((m.dest.y - m.src.y).abs <= 1 && (m.src.x - m.dest.x).abs <= 1) {
      cg.board(m.dest.x)(m.dest.y) match {
        case Some(p2) => {
          (Player.getPlayer(p), Player.getPlayer(p2)) match {
            case (White, White) | (Black, Black) => false
            case _ => true
          }
        }
        case None => true
      }
    } else {
      false
    }
  }
  def validBlackPawnMove(cg: ChessGame, p: Piece, m: Move): Boolean =  {
    if (m.src.y == 6 && m.dest.y == 4 && m.src.x == m.src.x) {
      (cg.board(m.dest.x)(m.dest.y), cg.board(m.dest.x)(m.dest.y+1)) match {
        case (None, None) => true
        case (_, _) => false
      }
    } else if (m.src.y - m.dest.y == 1 && m.src.x == m.dest.x) {
      cg.board(m.dest.x)(m.dest.y) match {
        case None => true
        case _ => false
      }
    } else if (m.src.y - m.dest.y == 1 && (m.src.x - m.dest.x).abs == 1) {
      cg.board(m.dest.x)(m.dest.y) match {
        case Some(p2) => {
          (Player.getPlayer(p), Player.getPlayer(p2)) match {
            case (Black, Black) => false
            case _ => true
          }
        }
        case None => false
      }
    } else {
      false
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
  def convert(s: String): Array[Array[Option[Piece]]] = {
    val p = List('♖', '♘', '♗', '♕', '♔', '♙', '♜', '♞',  '♝',  '♛',  '♚',  '♟',  '.')
    val l = {
      val tmp = s.filter(p.contains(_)).toCharArray.map(convertChar(_))
      if(tmp.length != 64) 
        "♜♞♝♛♚♝♞♜♟♟♟♟♟♟♟♟................................♙♙♙♙♙♙♙♙♖♘♗♕♔♗♘♖".toCharArray.map(convertChar(_))
      else
        tmp
    }
    val len = 8
    val arr = Array.ofDim[Option[Piece]](len, len)
    for(i <- 0 to len - 1) {
      for(j <- 0 to len - 1) {
        arr(i)(j) = l(len * (len-1-j) + i)
      }
    }
    arr
  }
}
case class ChessGame(val board: Array[Array[Option[Piece]]], val currentPlayer: Player, val status: Status) {
  def this(board: String, currentPlayer: Player) {
    this(ChessGame.convert(board), currentPlayer, Undecided)
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
  override def equals(that: Any): Boolean =
    that match {
      case that: ChessGame =>
        for(i <- 0 to this.board.length - 1){
          for(j <- 0 to this.board(i).length - 1){
            (this.board(i)(j), that.board(i)(j)) match {
              case (None, None) =>
              case (None, Some(_)) | (Some(_), None) => return false
              case (Some(a), Some(b)) => if(a != b) return false
            }
          }
        }
        return this.currentPlayer == that.currentPlayer
      case _ => return false
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
                case Stalemate(p) => println(s"Stalemate! $p can no longer move.}"); running = false
                case Checkmate(p) => println(s"${p} wins.}"); running = false
                case _ => 
              }
            case Left(x) => println(x)
          }
        }
        case "draw" | "d" =>
          cg.status match {
            case Undecided => cg = ChessGame(cg.board, cg.currentPlayer.opponent, DrawRequest)
            case DrawRequest => println("It's draw!"); running = false
            case _ =>
          }
        case "restart" | "r" => cg = ChessGame()
        case "exit" | "e" => running = false
        case "help" | "h" =>
          println("'d' or 'draw' to ask draw to opponent.")
          println("'r' or 'restart' to restart the game.")
          println("'e' or 'exit' to quit.")
          println("'h' or 'help' for help.")
        case _ => println("Invalid move syntax.")
      }
    }
  }
}
