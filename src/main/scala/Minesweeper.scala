import scala.annotation.tailrec
import scala.util.Random

trait Cell
case object Mine extends Cell
case object Safe extends Cell
case class Unknown(covers: Cell) extends Cell
case class Flag(covers: Cell) extends Cell
case class Unsafe(surrounding: Int) extends Cell

case class Point(x: Int, y: Int)
case class Board(width: Int, height: Int, cells: Map[Point, Cell])

object Minesweeper {
  
  def generate(numberOfMines: Int, width: Int, height: Int): Board = {
    val allPositions = positions(width, height)
    val positionedMines = Random.shuffle(allPositions)
      .take(numberOfMines)
      .map(_ -> Unknown(Mine))
    
    val mineMap = Map(positionedMines: _*)    
    val cells = allPositions.filterNot(mineMap.contains).map(_ -> Unknown(Safe))

    Board(width, height, mineMap ++ cells)
  }
  
  def reveal(p: Point, board: Board): Board = reveal(Set(p), Set.empty, board)
  
  def flag(p: Point, board: Board): Board = {
    val old = board.cells(p)
    Board(board.width, board.height, board.cells + (p -> Flag(old)))
  }
  
  @tailrec
  private def reveal(ps: Set[Point], revealed: Set[Point], board: Board): Board  = {
    val remaining = ps.diff(revealed)
    if (remaining.isEmpty) board
    else {
      val current = remaining.head
      val neighs = findNeighbours(current, board)
      val surroundedBy = neighs.map(board.cells(_)).map {
        case Unknown(o) => o
        case Flag(o) => o
        case c:Cell => c   
      }.count {
        case Mine => true
        case _ => false
      }

      val _ps = remaining ++ neighs
      val _revealed = revealed + current 
      val newCell = if (surroundedBy > 0) Unsafe(surroundedBy) else Safe
      val _board = new Board(board.width, board.height , board.cells + (current -> newCell))

      reveal(_ps, _revealed, _board)
    }      
  }
  
  private def findNeighbours(p: Point, board: Board) = for {
    dx <- -1 to 1
    dy <- -1 to 1
    nx = p.x + dx
    ny = p.y + dy
    if nx >= 0 && nx < board.width
    if ny >= 0 && ny < board.height
  } yield Point(nx, ny)
  
  private def positions(width: Int, height: Int) = {
    for {
      x <- 0 until width
      y <- 0 until height
    } yield Point(x, y)
  }
}
