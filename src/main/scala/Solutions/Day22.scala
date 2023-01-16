package AoC2022
package Solutions

import Helpers.Readers
import Helpers.Matrix
import Helpers.Utils.{splitAt, Point}


object Day22 {

  def main(args: Array[String]): Unit = {
    val data = Readers.readFile("Data/Day22.txt")
    val gridData = splitAt(data)(x => x == "").head
    val commandsData = splitAt(data)(x => x == "").tail.head.head
    val nRows = gridData.size
    val nCols = gridData.map(_.length).max
    val grid = Matrix.fromElement(nRows, nCols, ' ')
    gridData.zipWithIndex.foreach(row => row._1.zipWithIndex.foreach(
      el => {
        if (el._1 != ' ') {
          grid(row._2, el._2) = el._1
        }
      }
    ))
    val commands = commandsData.split("((?<=[RL])|(?=[RL]))").toList
    var pos = grid.find('.')
    print(grid)

  }

  case class State(position: Point, direction: Char) {
    def Turn(command: String): State = {
      val directions = List('U', 'R', 'D', 'L')
      if (command == "L") {
        val idx = directions.indexOf(direction) - 1
        if (idx < 0) State(position, directions(3))
        else State(position, directions(idx))
      }
      else if (command == "R") {
        val idx = (directions.indexOf(direction) + 1) % 4
        State(position, directions(idx))
      }
      else {
        throw new IllegalStateException("Illegal turn direction")
      }
    }

    def Walk(grid: Matrix[Char]): State = {
      val step = direction match {
        case 'L' => (0, -1)
        case 'R' => (0, 1)
        case 'U' => (-1, 0)
        case 'D' => (1, 0)
      }
      var current = position
      var next = ' '
      while (next == ' '){
        current += step
        if (current.x < 0) current = Point(grid.nrows - 1, current.y)
        if (current.y < 0) current = Point(current.x, grid.ncols - 1)
        if (current.x >= grid.nrows) current = Point(0, current.y)
        if (current.y >= grid.nrows) current = Point(current.x, 0)
        next = grid(current.x, current.y)
      }
      if (next == '.') State(current, direction)
      else State(position, direction)
    }

  }

}

