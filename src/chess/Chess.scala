package chess

object SquareType extends Enumeration {
  val TopLeft, Top, TopRight, Right, BottomRight, Bottom, BottomLeft, Left, Middle = Value
}

object SquareDimensions {
  // currently these have to be odd so that the chess piece can go in the square center
  // not sure if that's ideal
  val height = 3
  val width = 5
  val midHeight = height/2 + 1
}

object Color extends Enumeration {
  val Black, White = Value
}

case class Square(rank: Int, file: Char, color: Color.Value) {
  def draw() = {
    import Color._

    color match {
      case Black => "/"
      case White => " "
    }
  }
}

case class Row(rank: Int){
  import Color._

  val files = {
    ('a' to 'h') map { file =>
      if ((rank % 2 == 1 && file % 2 == 1) || (rank % 2 == 0 && file % 2 == 0))
        Square(rank, file, Black)
      else
        Square(rank, file, White)
    }
  }.toArray

  def draw(state: GameState.PieceLocations) = {
    import SquareDimensions._

    val edge = "-" * (width + 1) * 8
    println(edge)
    (1 to height) foreach { line =>
      ('a' to 'h') foreach { file =>
        print("|")
        if (line == midHeight) {
          print(files(file - 'a').draw() * (width/2))
          print(state getOrElse((file, rank), files(file - 'a').draw()))
          print(files(file - 'a').draw() * (width/2))
        } else {
          print(files(file - 'a').draw() * width)
        }
        if (file == 'h')
          print("|")
      }
      println()
    }
    if (rank == 1)
      println(edge)
    state
  }
}

object Board {
  import Color._

  val rows = {
    (1 to 8).reverse map Row
  }.toArray

  def draw(state: GameState.PieceLocations) = rows foreach { _.draw(state) }

  def isValidPosition(file: Char, rank: Int) =
    1 <= rank && 8 >= rank && 'a' <= file && 'h' >= file

  def newGame() =
    Map(('a', 1) -> Rook(White), ('b', 1) -> Knight(White), ('c', 1) -> Bishop(White),
    ('d', 1) -> Queen(White), ('e', 1) -> King(White), ('f', 1) -> Bishop(White), ('g', 1) ->
    Knight(White), ('h', 1) -> Rook(White), ('a', 8) -> Rook(Black), ('b', 8) ->
    Knight(Black), ('c', 8) -> Bishop(Black), ('d', 8) -> Queen(Black), ('e', 8) ->
    King(Black), ('f', 8) -> Bishop(Black), ('g', 8) -> Knight(Black), ('h', 8) ->
    Rook(Black))

}