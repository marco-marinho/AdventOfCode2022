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
    val pos = grid.find('.')
    var state = State(Point(pos._1, pos._2), 'R')
    for (command <- commands) {
      state = doCommand(command, state, grid)
    }
    println("Task 01: " + state.Score())

    val face_size = Math.sqrt((grid.ncols * grid.nrows - grid.countAll(' ')) / 6).toInt
    val creases = findOuterCreases(grid)
    println(face_size)
    println(creases)
  }

  private def doCommand(command: String, state: State, grid: Matrix[Char]): State = {
    if (command == "L" || command == "R") state.Turn(command)
    else {
      var current = state
      for (_ <- 0 until command.toInt) {
        current = current.Walk(grid)
      }
      current
    }
  }

  private def findOuterCreases(grid: Matrix[Char]): List[(Point, Int)] = {
    var creases = List[Point]()
    var quadrants = List[Int]()
    val offsets = List[Point](Point(1, 1), Point(0, 1), Point(1, 0), Point(0, 0))
    val quadrantsIndex = List[Int](4, 1, 3, 2)
    for (row <- 0 until grid.nrows - 1) {
      for (col <- 0 until grid.ncols - 1) {
        val elements = List[Char](grid(row, col), grid(row + 1, col),
          grid(row, col + 1), grid(row + 1, col + 1))
        val empty_count = elements.count(_ == ' ')
        if (empty_count == 1) {
          val offset = offsets(elements.indexOf(' '))
          creases = (Point(row, col) + offset) :: creases
          quadrants = quadrantsIndex(elements.indexOf(' ')) :: quadrants
        }
      }
    }
    creases.zip(quadrants)
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
      while (next == ' ') {
        current += step
        if (current.x < 0) current = Point(grid.nrows - 1, current.y)
        if (current.y < 0) current = Point(current.x, grid.ncols - 1)
        if (current.x >= grid.nrows) current = Point(0, current.y)
        if (current.y >= grid.ncols) current = Point(current.x, 0)
        next = grid(current.x, current.y)
      }
      if (next == '.') State(current, direction)
      else State(position, direction)
    }

    def Score(): Long = {
      val facing = direction match {
        case 'R' => 0
        case 'D' => 1
        case 'L' => 2
        case 'R' => 3
      }
      1000 * (position.x + 1) + 4 * (position.y + 1) + facing
    }

  }

}

