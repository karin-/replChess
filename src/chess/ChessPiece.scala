package chess
import Color._

abstract class ChessPiece {
  val color: Color.Value

  def isValidMove(oldFile: Char, oldRank: Int, newFile: Char, newRank: Int): Boolean
}

case class Pawn(color: Color.Value) extends ChessPiece{
  import Board._

  def isValidMove(newFile: Char, newRank: Int, oldFile: Char, oldRank: Int) =
    oldRank == newRank + 1 && oldFile == newFile 

  override def toString = color match{
    case White => "\u2659"
    case Black => "\u265F"
  }
}

case class Rook(color: Color.Value) extends ChessPiece{
  def isValidMove(newFile: Char, newRank: Int, oldFile: Char, oldRank: Int) = {
    (oldFile == newFile || oldRank == newRank) 
  }
  override def toString = color match{
    case White => "\u2656"
    case Black => "\u265C"
  }
}

case class Bishop(color: Color.Value) extends ChessPiece{
  def isValidMove(newFile: Char, newRank: Int, oldFile: Char, oldRank: Int) = {
    (newFile - oldFile).abs == (newRank - oldRank).abs 
  }
  override def toString = color match{
    case White => "\u2657"
    case Black => "\u265D"
  }
}

case class Queen(color: Color.Value) extends ChessPiece{
  def isValidMove(newFile: Char, newRank: Int, oldFile: Char, oldRank: Int) = {
    ((oldFile == newFile || oldRank == newRank) || (newFile - oldFile).abs == (newRank - oldRank).abs) 
  }
  override def toString = color match{
    case White => "\u2655"
    case Black => "\u265B"
  }
}

case class King(color: Color.Value) extends ChessPiece{
  def isValidMove(newFile: Char, newRank: Int, oldFile: Char, oldRank: Int) = {
    ((newFile - oldFile).abs == 1 && newRank == oldRank || (newRank - oldRank).abs == 1 && newFile == oldFile)
  }
  override def toString = color match{
    case White => "\u2654"
    case Black => "\u265A"
  }
}

case class Knight(color: Color.Value) extends ChessPiece{
  def isValidMove(newFile: Char, newRank: Int, oldFile: Char, oldRank: Int) = {
    val xDifferential = (newFile - oldFile).abs
    val yDifferential = (newRank - oldRank).abs
    ((xDifferential == 1 && yDifferential == 2) || (xDifferential == 2 && yDifferential == 1)) 
  }
  override def toString = color match{
    case White => "\u2658"
    case Black => "\u265E"
  }
}