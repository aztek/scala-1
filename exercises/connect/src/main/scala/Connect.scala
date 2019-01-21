import scala.annotation.tailrec

sealed trait Color extends Product with Serializable
object Color {
  type T = Color
  case object Black extends T
  case object White extends T
}

final case class Connect(input: List[String]) {
  type Cell = Option[Color]
  type Board = List[List[Cell]]

  // Assume that the input is well-found,
  // i.e. it's a rectangle with only 'X', 'O' and '.' in it
  val board: Board = input map (_.collect {
    case 'X' => Some(Color.Black)
    case 'O' => Some(Color.White)
    case '.' => None
  }.toList)

  private val height = board.length
  private val width  = board.headOption.fold(0)(_.length)

  def winner: Option[Color] =
    if (wins(Color.Black))
      Some(Color.Black)
    else if (wins(Color.White))
      Some(Color.White)
    else
      None

  type Pos = (Int, Int)
  private def neighbours(p: Pos): Set[Pos] = for {
    (dx, dy) <- Set((+1, 0), (-1, 0), (0, +1), (0, -1), (-1, +1), (+1, -1))
    (x, y) = p
    (x0, y0) = (x + dx, y + dy)
    if x0 >= 0 && x0 < height &&
       y0 >= 0 && y0 < width
  } yield (x0, y0)

  private def startingPosition: Color => Pos => Boolean = {
    case Color.White => { case (x, _) => x == 0 }
    case Color.Black => { case (_, y) => y == 0 }
  }

  private def endingPosition: Color => Pos => Boolean = {
    case Color.White => { case (x, _) => x == height - 1 }
    case Color.Black => { case (_, y) => y == width  - 1 }
  }

  private def wins(color: Color) = {
    val occs = occupations(color, board)
    val startSet = occs filter startingPosition(color)
    val closure = saturate(startSet, extendConnections(occs, _: Set[Pos]))
    closure exists endingPosition(color)
  }

  private def occupations(color: Color, board: Board): Set[Pos] = board.zipWithIndex.flatMap {
    case (row, rowIndex) => row.zipWithIndex.collect {
      case (Some(c), colIndex) if c == color => (rowIndex, colIndex)
    }
  }.toSet

  private def extendConnections(occs: Set[Pos], closure: Set[Pos]): Set[Pos] =
    closure flatMap neighbours diff closure intersect occs

  @tailrec
  private def saturate[A](set: Set[A], produce: Set[A] => Set[A]): Set[A] = {
    val extension = produce(set)
    if (extension.isEmpty) set else saturate(set union extension, produce)
  }
}