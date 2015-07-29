package chess

case class Pos(file: Char, rank: Int) {

  def up = Pos(file, rank + 1)

  def right = Pos((file + 1).toChar, rank)

  def left = Pos((file - 1).toChar, rank)

  def down = Pos(file, rank - 1)

  def onSameRow(other: Pos) = file == other.file

  def onSameCol(other: Pos) = rank == other.rank

  def onSameDiagonal(other: Pos) = (file - other.file).abs == (rank - other.rank).abs

  def isValid = 1 <= rank && 8 >= rank && 'a' <= file && 'h' >= file
}
