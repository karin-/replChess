package chess
import Color._

class Game(board: GameBoard, turn: Color.Value) {
  val nextTurn = turn match {
    case White => Black
    case Black => White
  }

  def makeMove(command: String) = {
    val re = """^([a-hA-H])([1-8]) ?to ?([a-hA-H])([1-f8])$""".r
    val matched = re.findFirstMatchIn(command)
    if (matched.isDefined) {
      val result = matched.get.subgroups
      val oldFile = result(0)(0)
      val oldRank = result(1).toInt
      val newFile = result(2)(0)
      val newRank = result(3).toInt
      History.add(this)
      new Game(board.move(oldFile, oldRank, newFile, newRank), nextTurn)
    } else {
      throw new RuntimeException("command not recognized")
    }
  }

  def undo() = {}

  def redo() = {}
}
