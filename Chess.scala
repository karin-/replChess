package chess

object SquareType extends Enumeration {
  val TopLeft, Top, TopRight, Right, BottomRight, Bottom, BottomLeft, Left, Middle = Value
}

object Color extends Enumeration {
  val Black, White = Value
}

case class Square(x: Int, y: Int, color: Color.Value, var contents: Option[ChessPiece] = None) {
  def draw() = {
    import Color._

    var toPrint = ""
    val occupant = contents match{
      case Some(piece) => piece.toString()
      case None => " "
    }

    if (y == 0) toPrint = "|   " + occupant + "   |" else toPrint = "   " + occupant + "    |"
    if (color == Black) {
      toPrint = toPrint.replace(" ", "/")
    }
    toPrint
  }
}

object Board {
  import Color._

  var capturedWhite = Array[ChessPiece]()
  var capturedBlack = Array[ChessPiece]()
  val squareHeight = 3
  val squares = Array.ofDim[Square](8,8)
  var currentColor = White

  for (x <- 0 until 8){
    for (y <- 0 until 8){
      squares(x)(y) = Square(x, y, currentColor)
      currentColor = if (currentColor == White) Black else White
    }
    currentColor = if (currentColor == White) Black else White
  }

  def draw() = {
    val edge = "-----------------------------------------------------------------------"
    for (row <- squares) {
      println(edge)
      for (i <- 1 to squareHeight) {
        for (square <- row) {
          print(square.draw())
        }
        println()
      }
    }
    println (edge)
  }

  def isValidPosition(x: Int, y: Int) : Boolean =
    y < squares.length &&
    y >= 0 &&
    x < squares(0).length &&
    x >= 0
}

//////////////////////////////////////////////////////////////////////////////////////


abstract class ChessPiece{
  val color: Color.Value
  var location: Square

  def isValidMove(x: Int, y: Int) : Boolean

  def moveTo(x: Int, y: Int) : Unit = {
    import Board._
    if (isValidMove(x, y)){
      location.contents = None
      val newLocation : Square = squares(x)(y)
      newLocation.contents match{
        case Some(piece) => if (piece.color == Color.Black) capturedBlack :+ piece else capturedWhite :+ piece
        case _ =>
      }
      newLocation.contents = Some(this)
      this.location = newLocation
    } else {
      throw new RuntimeException("invalid move")
    }
  }
}

case class Pawn(var location: Square, color: Color.Value) extends ChessPiece{
  def isValidMove(newX: Int, newY: Int) = {
    location.y == newY + 1 && location.x == newX && Board.isValidPosition(newX, newY)
  }
  override def toString() = "P"
}

case class Rook(var location: Square, color: Color.Value) extends ChessPiece{
  def isValidMove(newX: Int, newY: Int) = {
    (location.x == newX || location.y == newY) && Board.isValidPosition(newX, newY)
  }
  override def toString() = "R"
}

case class Bishop(var location: Square, color: Color.Value) extends ChessPiece{
  def isValidMove(newX: Int, newY: Int) = {
    (newX - location.x).abs == (newY - location.y).abs && Board.isValidPosition(newX, newY)
  }
  override def toString = "B"
}

case class Queen(var location: Square, color: Color.Value) extends ChessPiece{
  def isValidMove(newX: Int, newY: Int) = {
    ((location.x == newX || location.y == newY) || (newX - location.x).abs == (newY - location.y).abs) && Board.isValidPosition(newX, newY)
  }
  override def toString = "Q"
}

case class King(var location: Square, color: Color.Value) extends ChessPiece{
  def isValidMove(newX: Int, newY: Int) = {
    ((newX - location.x).abs == 1 && newY == location.y || (newY - location.y).abs == 1 && newX == location.x) &&
      Board.isValidPosition(newX, newY)
  }
  override def toString = "K"
}

case class Knight(var location: Square, color: Color.Value) extends ChessPiece{
  def isValidMove(newX: Int, newY: Int) = {
    val xDifferential = (newX - location.x).abs
    val yDifferential = (newY - location.y).abs
    ((xDifferential == 1 && yDifferential == 2) || (xDifferential == 2 && yDifferential == 1)) && Board.isValidPosition(newX, newY)
  }
  override def toString = "k"
}
