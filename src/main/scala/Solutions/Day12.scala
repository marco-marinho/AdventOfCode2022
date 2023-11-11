package AoC2022
package Solutions

import Helpers.Utils.Point
import Helpers.{Matrix, Readers}

import scala.collection.mutable

object Day12 {
    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day12.txt")
        val matrix = new Matrix(data.map(_.toCharArray).toArray)
        val start = new Point(matrix.find('S'))
        val finish = new Point(matrix.find('E'))
        val path = pathFind(start, finish, matrix)
        val lowestElevation = matrix.findAll('a')
        println("Task 01: " + path)
        println("Task 02: " + lowestElevation.map(coords => pathFind(new Point(coords), finish, matrix)).min)
    }

    private def pathFind(start: Point, end: Point, field: Matrix[Char]): Int = {
        val toVisit = mutable.PriorityQueue[Path]()
        toVisit.addOne(Path(start, 0, List[Point]()))
        var visited = Set[Point]()
        while (toVisit.nonEmpty) {
            val curr = toVisit.dequeue()
            if (visited.contains(curr.coords)) {}
            else {
                val currDist = curr.dist
                val currCord = curr.coords
                val currPath = curr.path
                visited += currCord
                if (currCord == end) return {
                    currDist
                }
                val currEl = transformElevation(field(currCord.x, currCord.y))
                val neighbours = field.neighboursCoords(currCord.x, currCord.y)
                val elNeighbours = field.neighbours(currCord.x, currCord.y)
                for (entry <- neighbours.zip(elNeighbours)) {
                    val neighEl = transformElevation(entry._2)
                    val diff = neighEl - currEl
                    if (diff > 1 || visited.contains(new Point(entry._1))) {}
                    else {
                        val newPath = currCord :: currPath
                        toVisit.addOne(Path(new Point(entry._1), currDist + 1, newPath))
                    }
                }
            }
        }
        Int.MaxValue
    }

    private def transformElevation(input: Char): Int = {
        input match {
            case 'S' => 0
            case 'E' => 'z' - 'a'
            case _ => input - 'a'
        }
    }

    case class Path(coords: Point, dist: Int, path: List[Point]) extends Ordered[Path] {
        def this(coordT: (Int, Int), distT: Int, pathT: List[Point]) = this(Point(coordT._1, coordT._2), distT, pathT)

        def compare(that: Path): Int = that.dist compare this.dist
    }

}


