import scala.util.Random

trait Cell
case object Mine extends Cell
case class Unknown(covers: Cell) extends Cell
case class Flag(covers: Cell) extends Cell
case class Safe(surrounding: Int) extends Cell

class Minesweeper() {

}

object Minesweeper {
  def generate(numberOfMines: Int, width: Int, height: Int): Minesweeper = {
    val dim = width * height
    val mines = Stream.continually(Mine).take(numberOfMines)
    val random = Random.shuffle(mines)

    new Minesweeper()
  }
}
