package chess
import Color._
import Board._


class GameBoard(pieces: Map[(Char, Int), ChessPiece]){

  def isValidMove(oldFile: Char, oldRank: Int, newFile: Char, newRank: Int) = {
    if (!isEmptyPos(oldFile, oldRank)){
      val pieceToMove = pieces(oldFile, oldRank)
      pieceToMove.isValidMove(oldFile, oldRank, newFile, newRank) && (pieceToMove.color != pieces(newFile, newRank).color)
    } else {
      false
    }
  }

  def isEmptyPos(file: Char, rank: Int) = pieces contains (file, rank)

  def move(oldFile: Char, oldRank: Int, newFile: Char, newRank: Int) = {
    if (isValidMove(oldFile: Char, oldRank: Int, newFile: Char, newRank: Int)) {
      val pieceToMove = pieces(oldFile, oldRank)
      // make capture if necessary
      new GameBoard(pieces + ((newFile, newRank) -> pieceToMove))
    } else {
      throw new RuntimeException("illegal move")
    }
  }
}