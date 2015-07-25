package chess

/**
 * keeps track of last few game states and undos to allow undo/redo
 */
object History {
  val lastBoards = Array[Game]()
  val undos = Array[Game]()

  def add(game: Game) = {
    lastBoards :+ game
  }

}
