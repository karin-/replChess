package chess


case class GameBoard(pieces: Map[Pos, ChessPiece], captures: Array[ChessPiece]) {

  def isValidMove(oldPos: Pos, newPos: Pos) = {
    val pieceToMove = pieces(oldPos)
    pieceToMove.isValidMove(oldPos, newPos) &&
    newPos.isValid &&
    Some(pieceToMove.color) != pieces.get(newPos).map(_.color)
  }

  def move(oldPos: Pos, newPos: Pos) = {
    if (isValidMove(oldPos: Pos, newPos: Pos)) {
      val pieceToMove = pieces(oldPos)
      // History.add
      pieceToMove match {
        case p: Pawn => val captured = pawnCapture(p, newPos); new GameBoard(pieces - captured - oldPos + (newPos -> pieceToMove), captures)
        case _ => if (pieces.get(newPos).isDefined) captures :+ pieces(newPos); new GameBoard(pieces - oldPos + (newPos -> pieceToMove), captures)
      }
    } else {
      throw new RuntimeException("illegal move")
    }
  }

  def pawnCapture(pieceToMove: Pawn, pos: Pos) = {
    import Color._

    def helper(firstDiag: Pos, secondDiag: Pos) = {
      if (pieces.get(firstDiag).isDefined && pieces.get(secondDiag).isDefined) {
        captures :+ pieces(secondDiag)
        secondDiag // choose between right and left
      }
      else if (pieces.get(firstDiag).isDefined) {
        captures :+ pieces(firstDiag)
        firstDiag
      }
      else if (pieces.get(secondDiag).isDefined) {
        captures :+ pieces(secondDiag)
        secondDiag
      }
      else {
        Pos('z',10)
      }
    }
    
    pieceToMove.color match {
      case White => helper(pos.up.left, pos.up.right)
      case Black => helper(pos.down.left, pos.down.right)
    }
  }

  def draw() = {
    val rows = {
      (1 to 8).reverse map Row
    }.toArray

    rows foreach {
      _.draw(pieces)
    }
  }
}

object GameBoard {
  import Color._

  def newBoard() = {
    val newPieces = Map(
    Pos('a', 1) -> Rook(White), Pos('b', 1) -> Knight(White), Pos('c', 1) -> Bishop(White), Pos('d', 1) -> Queen(White),
    Pos('e', 1) -> King(White), Pos('f', 1) -> Bishop(White), Pos('g', 1) -> Knight(White), Pos('h', 1) -> Rook(White),
    Pos('a', 2) -> Pawn(White), Pos('b', 2) -> Pawn(White), Pos('c', 2) -> Pawn(White), Pos('d', 2) -> Pawn(White),
    Pos('e', 2) -> Pawn(White), Pos('f', 2) -> Pawn(White), Pos('g', 2) -> Pawn(White), Pos('h', 2) -> Pawn(White),
    Pos('a', 7) -> Pawn(Black), Pos('b', 7) -> Pawn(Black), Pos('c', 7) -> Pawn(Black), Pos('d', 7) -> Pawn(Black),
    Pos('e', 7) -> Pawn(Black), Pos('f', 7) -> Pawn(Black), Pos('g', 7) -> Pawn(Black), Pos('h', 7) -> Pawn(Black),
    Pos('a', 8) -> Rook(Black), Pos('b', 8) -> Knight(Black), Pos('c', 8) -> Bishop(Black), Pos('d', 8) -> Queen(Black),
    Pos('e', 8) -> King(Black), Pos('f', 8) -> Bishop(Black), Pos('g', 8) -> Knight(Black), Pos('h', 8) -> Rook(Black))

    new GameBoard(newPieces, Array[ChessPiece]())
  }
}

object Color extends Enumeration {
  val Black, White = Value
}

case class Square(color: Color.Value) {
  def draw() = {
    import Color._

    color match {
      case Black => "/"
      case White => " "
    }
  }
}

object SquareDimensions {
  // currently these have to be odd so that the chess piece can go in the square center
  // not sure if that's ideal
  val height = 3
  val width = 5
  val midHeight = height/2 + 1
}

case class Row(rank: Int){
  import Color._

  val files = {
    ('a' to 'h') map { file =>
      if ((rank % 2 == 1 && file % 2 == 1) || (rank % 2 == 0 && file % 2 == 0))
        Square(Black)
      else
        Square(White)
    }
  }.toArray

  def draw(pieces: Map[Pos, ChessPiece]) = {
    import SquareDimensions._

    val edge = "-" * (width + 1) * 8
    println(edge)
    (1 to height) foreach { line =>
      ('a' to 'h') foreach { file =>
        print("|")
        if (line == midHeight) {
          print(files(file - 'a').draw() * (width/2))
          print(pieces getOrElse(Pos(file, rank), files(file - 'a').draw()))
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
  }
}
