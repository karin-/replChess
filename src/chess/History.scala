package chess
import scala.collection.mutable.ArrayStack

/**
 * keeps track of last few game states and undos to allow undo/redo
 */
object History {
  val lastBoards = ArrayStack[Game]()

  def add(game: Game) = lastBoards.push(game)

  def undo() = lastBoards.pop()

  def clear() = lastBoards.clear()
}
