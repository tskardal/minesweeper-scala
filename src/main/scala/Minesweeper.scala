import scala.util.Random

trait Cell
case object Mine extends Cell
case object Safe extends Cell
case class Unknown(covers: Cell) extends Cell
case class Flag(covers: Cell) extends Cell
case class Unsafe(surrounding: Int) extends Cell

/** Minesweeper
  * 
  * Brett som består av celler
  * Celle angis av x- og y-koordinat
  * Hvis en celle er safe må vi ekspandere rekursivt til vi treffer en unsafe
  */

class Minesweeper(mines: Set[(Int, Int)], board: Map[(Int, Int), Cell]) {
  val hoist: List[List[Cell]] = Nil
  hoist.updated(2, hoist(2).updated(1, Safe))
}

object Minesweeper {
  def generate(numberOfMines: Int, width: Int, height: Int): Minesweeper = {    
    val mines =  Stream.continually(Mine).take(numberOfMines)
    val random = Random.shuffle(mines)
    
    val hoist = Random.shuffle(positions(width, height))
      .take(5)
      .map(p => p -> Mine) // _ -> Mine

    null
  }
  
  private def positions(width: Int, height: Int) = {
    for {
      x <- 0 until width
      y <- 0 until height
    } yield (x, y)
  }
}
