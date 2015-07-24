package chess

object SquareType extends Enumeration {
  val TopLeft, Top, TopRight, Right, BottomRight, Bottom, BottomLeft, Left, Middle = Value
}

object SquareDimensions {
  val height = 3
  val width = 5
  val midHeight = 2
  val midWidth = 3
}

object Color extends Enumeration {
  val Black, White = Value
}

case class Square(rank: Int, file: Char, color: Color.Value) {
  def draw() = {
    import Color._

    color match{
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

  def draw() = {
    import SquareDimensions._

    val edge = "-" * (width + 1) * 8
    println(edge)
    (1 to height) foreach { line =>
      ('a' to 'h') foreach { file =>
        print("|")
        if (line == midHeight) {
          print(files(file - 'a').draw() * (width/2))
          print(Board.boardState getOrElse((file, rank), files(file - 'a').draw()))
          print(files(file - 'a').draw() * (width/2))
        }else{
          print(files(file - 'a').draw() * width)
        }
        if (file == 'h')
          print("|")
      }
      println()
    }
    if (rank == 1)
      println(edge)
  }
}

object Board {
  import Color._

  val captured = Array[ChessPiece]()
  val ranks = {
    (1 to 8).reverse map Row
  }.toArray
  val boardState = scala.collection.mutable.Map[(Char, Int), ChessPiece]()

  def draw() = ranks foreach { _.draw() }

  def isValidPosition(rank: Char, file: Int) : Boolean =
    1 <= file &&
    8 >= file &&
    'a' <= rank &&
    'h' >= rank

  def newGame() = {
    boardState += (('a', 1) -> Rook('a', 1, White), ('b', 1) -> Knight('b', 1, White), ('c', 1) -> Bishop('c', 1, White),
      ('d', 1) -> Queen('d', 1, White), ('e', 1) -> King('e', 1, White), ('f', 1) -> Bishop('f', 1, White), ('g', 1) ->
      Knight('g', 1, White), ('h', 1) -> Rook('h', 1, White), ('a', 8) -> Rook('a', 8, Black), ('b', 8) ->
      Knight('b', 8, Black), ('c', 8) -> Bishop('c', 8, Black), ('d', 8) -> Queen('d', 8, Black), ('e', 8) ->
      King('e', 8, Black), ('f', 8) -> Bishop('f', 8, Black), ('g', 8) -> Knight('g', 8, Black), ('h', 8) ->
      Rook('h', 8, Black))

    Array(2, 7) foreach { rank =>
      ('a' to 'h') foreach { file =>
        rank match{
          case 2 => boardState += ((file, rank) -> Pawn(file, rank, White))
          case 7 => boardState += ((file, rank) -> Pawn(file, rank, Black))
        }
      }
    }
  }

  def makeMove(command: String) = {
    val re = """^([a-hA-H])([1-8]) ?to ?([a-hA-H])([1-8])$""".r
    val matched = re.findFirstMatchIn(command)
    if (matched == None) {
      throw new RuntimeException("command not recognized")
    } else {
      val result = matched.get.subgroups
      val oldFile = result(0)(0)
      val oldRank = result(1).toInt
      val newFile = result(2)(0)
      val newRank = result(3).toInt
      if (!boardState.contains((oldFile, oldRank))) {
        println("There is no piece at " + oldFile + oldRank)
      } else {
        boardState(oldFile, oldRank).moveTo(newFile, newRank)
      }
    }
  }
}

//////////////////////////////////////////////////////////////////////////////////////


abstract class ChessPiece {
  val color: Color.Value
  var file: Char
  var rank: Int

  def isValidMove(file: Char, rank: Int): Boolean

  def moveTo(newFile: Char, newRank: Int) : Unit = {
    import Board._
    if (isValidMove(newFile, newRank)){
      boardState remove (file, rank)
      if (boardState contains (newFile, newRank)) captured :+ boardState.get(newFile, newRank)
      boardState update ((newFile, newRank), this)
      this.rank = newRank
      this.file = newFile
    } else {
      throw new RuntimeException("invalid move")
    }
  }
}

case class Pawn(var file: Char, var rank: Int, color: Color.Value) extends ChessPiece{
  import Board._
  import Color._

  def isValidMove(newFile: Char, newRank: Int) = {
    rank == newRank + 1 && file == newFile && Board.isValidPosition(newFile, newRank) &&
      !(boardState contains (newFile, newRank))
  }

  override def moveTo(newFile: Char, newRank: Int) : Unit = {
    if (isValidMove(newFile, newRank)){
      boardState update ((newFile, newRank), this)
      val possibleCaptures = (boardState.get(((newFile + 1).toChar, newRank + 1)),
        boardState.get(((newFile - 1).toChar, newRank + 1)))
      possibleCaptures match{
        case (Some(piece1), Some(piece2)) => //which would you like to capture?
        case (Some(piece1), None) => boardState remove ((newFile + 1).toChar, newRank + 1); captured :+ piece1
        case (None, Some(piece2)) => boardState remove ((newFile - 1).toChar, newRank + 1); captured :+ piece2
        case (None, None) =>
      }
    }
  }
  override def toString = color match{
    case White => "\u2659"
    case Black => "\u265F"
  }
}

case class Rook(var file: Char, var rank: Int, color: Color.Value) extends ChessPiece{
  def isValidMove(newFile: Char, newRank: Int) = {
    (file == newFile || rank == newRank) && Board.isValidPosition(newFile, newRank)
  }
  override def toString = color match{
    case Color.White => "\u2656"
    case Color.Black => "\u265C"
  }
}

case class Bishop(var file: Char, var rank: Int, color: Color.Value) extends ChessPiece{
  def isValidMove(newFile: Char, newRank: Int) = {
    (newFile - file).abs == (newRank - rank).abs && Board.isValidPosition(newFile, newRank)
  }
  override def toString = color match{
    case Color.White => "\u2657"
    case Color.Black => "\u265D"
  }
}

case class Queen(var file: Char, var rank: Int, color: Color.Value) extends ChessPiece{
  def isValidMove(newFile: Char, newRank: Int) = {
    ((file == newFile || rank == newRank) || (newFile - file).abs == (newRank - rank).abs) && Board.isValidPosition(newFile, newRank)
  }
  override def toString = color match{
    case Color.White => "\u2655"
    case Color.Black => "\u265B"
  }
}

case class King(var file: Char, var rank: Int, color: Color.Value) extends ChessPiece{
  def isValidMove(newFile: Char, newRank: Int) = {
    ((newFile - file).abs == 1 && newRank == rank || (newRank - rank).abs == 1 && newFile == file) &&
      Board.isValidPosition(newFile, newRank)
  }
  override def toString = color match{
    case Color.White => "\u2654"
    case Color.Black => "\u265A"
  }
}

case class Knight(var file: Char, var rank: Int, color: Color.Value) extends ChessPiece{
  def isValidMove(newFile: Char, newRank: Int) = {
    val xDifferential = (newFile - file).abs
    val yDifferential = (newRank - rank).abs
    ((xDifferential == 1 && yDifferential == 2) || (xDifferential == 2 && yDifferential == 1)) && Board.isValidPosition(newFile, newRank)
  }
  override def toString = color match{
    case Color.White => "\u2658"
    case Color.Black => "\u265E"
  }
}