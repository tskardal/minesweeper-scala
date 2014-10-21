import org.scalatest.FunSuite

class MinesweeperTest extends FunSuite {
  test("creating a new game with a given dimension") {
    val game = new Minesweeper(5, 5)
    val allCells = game.board.values
    assert(allCells.size === 5 * 5)
  }

  test("a fresh game contains only unknown cells") {
    val game = new Minesweeper(5, 5)
    val allCells = game.board.values
    assert(allCells.forall({
      case u: Unknown => true
      case _ => false
    }))
  }
  
  test("revealing an unknown cell gives a board with fewer unknown cells") {
    val game = new Minesweeper(5, 5)    
    val before = game.board.values.count({
      case u: Unknown => true
      case _ => false
    })
    game.reveal(Point(0, 0))
    val after = game.board.values.count {
      case u: Unknown => true
      case _ => false
    }
    
    assert(after < before)
  }
  
  test("revealing a safe cell also reveal its neighbours") {
    val game = new Minesweeper(5, 5)
    game.reveal(Point(0, 0))
    val safeCells = game.board.values.count(_ == Safe)
    assert(safeCells === 5 * 5)
  }
  
  test("revealing a cell surrounded by two mines") {
    val game = new Minesweeper(3, 5)
    game.reveal(Point(0, 0))
    assert(game.board(Point(0,0)) == Unsafe(2))
  }
}
  