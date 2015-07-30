package chess
import Color._

case class Game(board: GameBoard, turn: Color.Value) {
  val nextTurn = turn match {
    case White => Black
    case Black => White
  }

  def move(command: String) = {
    val re = """^([a-hA-H])([1-8]) ?to ?([a-hA-H])([1-f8])$""".r
    val matched = re.findFirstMatchIn(command)

    matched match {
      case Some(move) => ???
      case None => throw new Exception()
    }
    if (matched.isDefined) {
      val result = matched.get.subgroups
      val oldPos = Pos(result(0)(0), result(1).toInt)
      val newPos = Pos(result(2)(0), result(3).toInt)
      if (board.pieces.get(oldPos).map(_.color).get == turn) {
        History.add(this)
        new Game(board.move(oldPos, newPos), nextTurn)
      } else {
        throw new RuntimeException(s"$oldPos does not contain a $turn piece")
      }
    } else {
      throw new RuntimeException("command not recognized")
    }
  }

  def draw() = {
    println(s"$turn's turn")
    board.draw()
  }
}

object Game {
  def newGame() = {
    History.clear()
    new Game(GameBoard.newBoard(), White)
  }
}
