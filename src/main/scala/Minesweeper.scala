import scala.annotation.tailrec
import scala.util.Random

trait Cell
trait Revealed extends Cell
case object Mine extends Revealed
case object Safe extends Revealed
case class Unsafe(surrounding: Int) extends Revealed
case class Unknown(covers: Revealed) extends Cell
case class Flag(covers: Revealed) extends Cell

case class Point(x: Int, y: Int)
case class Board(width: Int, height: Int, cells: Map[Point, Cell])

object Minesweeper {
  
  def generate(numberOfMines: Int, width: Int, height: Int): Board = {
    val rndPos = Random.shuffle(positions(width, height))
    val mines = rndPos.take(numberOfMines).map(_ -> Unknown(Mine))
    val safes = rndPos.drop(numberOfMines).map(_ -> Unknown(Safe))
    Board(width, height, mines.toMap ++ safes)
  }
  
  def reveal(p: Point, board: Board): Board = reveal(Set(p), Set.empty, board)
  
  def flag(p: Point, board: Board): Board = board.cells(p) match {
      case Unknown(covers) => Board(board.width, board.height, board.cells + (p -> Flag(covers)))
      case Flag(covers) => Board(board.width, board.height, board.cells + (p -> Unknown(covers)))
      case _ => board
  }

  @tailrec
  private def reveal(ps: Set[Point], revealed: Set[Point], board: Board): Board = {
    val remaining = ps.diff(revealed)
    remaining.toList match {
      case Nil => board
      case (r: Revealed) :: rest => reveal(rest.toSet, revealed + r, board)
      case c :: rest => {
        val neighs = findNeighbours(c, board)
        val nearbyMines = neighs.count {
          case (p, Mine) => true
          case (p, Unknown(Mine)) => true
          case (p, c:Cell) => false
        }
        val afterReveal = if (nearbyMines > 0) Unsafe(nearbyMines) else Safe
        val newBoard = Board(board.width, board.height, board.cells + (c -> afterReveal))
        reveal(
          remaining ++ neighs.map { case (p, c) => p },
          revealed + c,
          newBoard)
      }
    }
  }
  
  private def findNeighbours(p: Point, board: Board) = for {
    dx <- -1 to 1
    dy <- -1 to 1
    nx = p.x + dx
    ny = p.y + dy
    if nx >= 0 && nx < board.width
    if ny >= 0 && ny < board.height
    point = Point(nx, ny)
  } yield (point, board.cells(point))
  
  private def positions(width: Int, height: Int) = {
    for {
      x <- 0 until width
      y <- 0 until height
    } yield Point(x, y)
  }
}
