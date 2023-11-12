package AoC2022
package Solutions

import Helpers.Utils.{Point, splitAt}
import Helpers.{Matrix, Readers}


object Day22 {

    private type DoublePoint = (Point, Point)
    private type PointDirection = (Point, Char)

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

        val creases = findOuterCreases(grid)
        val connections = stitchFaces(grid, creases)

        var state_cube = State(Point(pos._1, pos._2), 'R')
        for (command <- commands) {
            state_cube = doCommandCube(command, state_cube, grid, connections)
        }
        println("Task 02: " + state_cube.Score())
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

    private def doCommandCube(command: String, state: State, grid: Matrix[Char],
                              stitches: Map[DoublePoint, PointDirection]): State = {
        if (command == "L" || command == "R") state.Turn(command)
        else {
            var current = state
            for (_ <- 0 until command.toInt) {
                current = current.WalkCube(grid, stitches)
            }
            current
        }
    }

    private def getStep(direction: Char): (Int, Int) = {
        val step = direction match {
            case 'L' => (0, -1)
            case 'R' => (0, 1)
            case 'U' => (-1, 0)
            case 'D' => (1, 0)
        }
        step
    }

    private def walk(point: Point, direction: Char): Point = {
        val step = getStep(direction)
        point + step
    }

    private def turn(grid: Matrix[Char], position: Point, direction: Char): (Boolean, Char) = {
        if (grid.coordinateValid(walk(position, direction)) && grid(walk(position, direction)) != ' ') {
            return (false, direction)
        }
        val next_dir = direction match {
            case 'U' | 'D' =>
                if (grid.coordinateValid(position + (0, -1)) && grid(position + (0, -1)) != ' ') (true, 'L')
                else (true, 'R')
            case 'L' | 'R' =>
                if (grid.coordinateValid(position + (-1, 0)) && grid(position + (-1, 0)) != ' ') (true, 'U')
                else (true, 'D')
            case _ => throw new IllegalStateException("Invalid state for turn")
        }
        next_dir
    }

    private def stitchFaces(grid: Matrix[Char], creases: List[(Point, Int)]): Map[DoublePoint, PointDirection] = {
        var connections = Map[DoublePoint, PointDirection]()
        for (crease <- creases) {
            val (positions, directions) = crease._2 match {
                case 1 => ((crease._1 + (0, -1), crease._1 + (1, 0)), ('L', 'D'))
                case 2 => ((crease._1 + (1, 0), crease._1 + (0, 1)), ('D', 'R'))
                case 3 => ((crease._1 + (-1, 0), crease._1 + (0, 1)), ('U', 'R'))
                case 4 => ((crease._1 + (0, -1), crease._1 + (-1, 0)), ('L', 'U'))
                case _ => throw new IllegalStateException("Invalid quadrant for crease.")
            }
            connections = connections ++ stitchOnce(grid, positions, directions)
        }
        connections
    }

    private def stitchOnce(grid: Matrix[Char], positions: DoublePoint,
                           directions: (Char, Char)): Map[DoublePoint, PointDirection] = {
        val face_size = Math.sqrt((grid.ncols * grid.nrows - grid.countAll(' ')) / 6).toInt
        var position_1 = positions._1
        val direction_1 = directions._1
        var position_2 = positions._2
        val direction_2 = directions._2
        val inputEntry = (position: Point, direction: Char) => (position, getOffgrid(grid, position, direction))
        val outputEntry = (position: Point, direction: Char) => (position, getNewDirection(grid, position, direction))
        var output = Map[DoublePoint, PointDirection]()
        for (i <- 0 until face_size) {
            output = output + (inputEntry(position_1, direction_1) -> outputEntry(position_2, direction_2))
            output = output + (inputEntry(position_2, direction_2) -> outputEntry(position_1, direction_1))
            if (i < face_size - 1) {
                position_1 = walk(position_1, direction_1)
                position_2 = walk(position_2, direction_2)
            }
            else {
                val (turn_1, dir_1) = turn(grid, position_1, direction_1)
                val (turn_2, dir_2) = turn(grid, position_2, direction_2)
                if (turn_1 && turn_2) return output
                if (!turn_1) position_1 = walk(position_1, direction_1)
                if (!turn_2) position_2 = walk(position_2, direction_2)
                output = output ++ stitchOnce(grid, (position_1, position_2), (dir_1, dir_2))
            }
        }
        output
    }

    private def getNewDirection(grid: Matrix[Char], position: Point, direction: Char): Char = {
        val output = direction match {
            case 'U' | 'D' =>
                if (!grid.coordinateValid(position + (0, 1)) || grid(position + (0, 1)) == ' ') 'L'
                else 'R'
            case 'L' | 'R' =>
                if (!grid.coordinateValid(position + (1, 0)) || grid(position + (1, 0)) == ' ') 'U'
                else 'D'
        }
        output
    }

    private def getOffgrid(grid: Matrix[Char], position: Point, direction: Char): Point = {
        val neighbours = perpendicularNeighbours(position, direction)
        if (!grid.coordinateValid(neighbours._1) || grid(neighbours._1) == ' ') neighbours._1
        else neighbours._2
    }

    private def perpendicularNeighbours(point: Point, direction: Char): DoublePoint = {
        val neighbours = direction match {
            case 'U' | 'D' => (point + (0, 1), point + (0, -1))
            case 'L' | 'R' => (point + (1, 0), point + (-1, 0))
            case _ => throw new IllegalStateException("Illegal direction for perpendicular neighbour")
        }
        neighbours
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
            val step = getStep(direction)
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

        def WalkCube(grid: Matrix[Char], stitches: Map[DoublePoint, PointDirection]): State = {
            val step = getStep(direction)
            if (stitches.contains((position, position + step))) {
                val (next, next_dir) = stitches((position, position + step))
                if (grid(next) == '.') State(next, next_dir)
                else State(position, direction)
            }
            else {
                if (grid(position + step) == '.') State(position + step, direction)
                else State(position, direction)
            }
        }

        def Score(): Long = {
            val facing = direction match {
                case 'R' => 0
                case 'D' => 1
                case 'L' => 2
                case 'U' => 3
            }
            1000 * (position.x + 1) + 4 * (position.y + 1) + facing
        }

    }

}

