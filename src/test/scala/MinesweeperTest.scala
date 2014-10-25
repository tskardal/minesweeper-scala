import org.scalatest.FunSuite

class MinesweeperTest extends FunSuite {
  test("creating a new game with a given dimension") {
    val board = Minesweeper.newGame(5, 5, 5)
    assert(board.cells.size === 5 * 5)
  }

  test("a fresh game contains only unknown cells") {
    val board = Minesweeper.newGame(5, 5, 5)    
    assert(board.cells.values.forall({
      case u: Unknown => true
      case _ => false
    }))
  }
  
  test("revealing an unknown cell gives a board with fewer unknown cells") {
    val b1 = Minesweeper.newGame(5, 5, 5)
    val before = b1.cells.values.count({
      case u: Unknown => true
      case _ => false
    })
    val b2 = Minesweeper.reveal(Point(0, 0), b1)
    val after = b2.cells.values.count {
      case u: Unknown => true
      case _ => false
    }
    
    assert(after < before)
  }
  
  test("revealing a safe cell also reveal its neighbours") {
    val board = Minesweeper.newGame(0, 5, 5)
    val after = Minesweeper.reveal(Point(0, 0), board)
    val safeCells = after.cells.values.count(_ == Safe)
    assert(safeCells === 5 * 5)
  }
  
  test("revealing a cell surrounded by mines displays the number of surrounding mines") {
    val cells = Map(
      Point(0, 0) -> Unknown(Safe),
      Point(0, 1) -> Unknown(Mine),
      Point(1, 0) -> Unknown(Safe),
      Point(1, 1) -> Unknown(Mine)
    )
    val result = Minesweeper.reveal(Point(0, 0), Board(2, 2, cells))
    assert(result.cells(Point(0,0)) == Unsafe(2))
  }
  
  test("flagging an unknown cell") {
    val cells = Map(
      Point(0, 0) -> Unknown(Safe),
      Point(0, 1) -> Unknown(Mine),
      Point(1, 0) -> Unknown(Safe),
      Point(1, 1) -> Unknown(Mine)
    )
    val board: Board = Board(2, 2, cells)
    val point: Point = Point(0, 0)    
    val result = Minesweeper.flag(point, board)
    assert(result.cells(point) == Flag(Safe))
  }

  test("flagging a flagged cell removes the flag") {
    val cells = Map(
      Point(0, 0) -> Flag(Safe),
      Point(0, 1) -> Unknown(Mine),
      Point(1, 0) -> Unknown(Safe),
      Point(1, 1) -> Unknown(Mine)
    )
    val point: Point = Point(0, 0)
    val board: Board = Board(2, 2, cells)
    val result = Minesweeper.flag(point, board)
    assert(result.cells(point) == Unknown(Safe))
  }
  
  test("flagging a revealed cell results in the same board") {
    val cells = Map(
      Point(0, 0) -> Safe,
      Point(0, 1) -> Unknown(Mine),
      Point(1, 0) -> Unknown(Safe),
      Point(1, 1) -> Unknown(Mine)
    )
    val board: Board = Board(2, 2, cells)
    val result = Minesweeper.flag(Point(0, 0), board)
    assert(board == result)
  }
}
  