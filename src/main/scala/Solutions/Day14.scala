package AoC2022
package Solutions

import Helpers.{Matrix, Readers}
import Helpers.Utils.Point

object Day14 {

    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day14.txt")
        val lines = data.map(_.split(" -> ").map(_.split(",")).map(x => Point(x(0).toInt, x(1).toInt)).toList)
        val parameters = getParameters(lines, 175)
        val grid = genGrid(lines, parameters)
        while (drip(grid, parameters)) {}
        val grid2 = genGrid(lines, parameters, task2 = true)
        while (drip(grid2, parameters)) {}
        println("Task 01: " + grid.countAll('o'))
        println("Task 02: " + grid2.countAll('o'))
    }

    private def genGrid(walls: List[List[Point]], parameters: Parameters, task2: Boolean = false): Matrix[Char] = {
        val grid = Matrix.fromElement(parameters.maxY + 1 + parameters.borderY,
            parameters.maxX - parameters.minX + 1 + parameters.borderX * 2, '.')
        walls.foreach(addWalls(_, grid, parameters))
        grid(0, 500 - parameters.minX + parameters.borderX) = '+'
        if (task2) {
            for (i <- 0 until grid.ncols) {
                grid(parameters.maxY + 2, i) = '#'
            }
        }
        grid
    }

    private def addWalls(limits: List[Point], matrix: Matrix[Char], parameter: Parameters): Unit = {
        val pairs = limits.sliding(2).toList
        pairs.foreach(x => {
            val start = x.head
            val stop = x(1)
            for (i <- Math.min(start.y, stop.y) to Math.max(start.y, stop.y);
                 j <- Math.min(start.x, stop.x) to Math.max(start.x, stop.x)) {
                matrix(i, j - parameter.minX + parameter.borderX) = '#'
            }
        })
    }

    private def drip(grid: Matrix[Char], parameters: Parameters): Boolean = {
        var cur = Point(0, 500 - parameters.minX + parameters.borderX)
        while (cur != Point(0, 0)) {
            if (cur.x == parameters.maxY + parameters.borderY) return false
            val possibilities = List[Point](cur + (1, 0), cur + (1, -1), cur + (1, 1))
            val objects = possibilities.map(it => grid(it.x, it.y))
            val next = objects.zip(possibilities).find(it => it._1 == '.')
            cur = next match {
                case Some(entry) => entry._2
                case None =>
                    grid(cur.x, cur.y) = 'o'
                    if (cur == Point(0, 500 - parameters.minX + parameters.borderX)) return false
                    Point(0, 0)
            }
        }
        true
    }

    private def getParameters(lines: List[List[Point]], extension: Int): Parameters = {
        val Xs = lines.flatten.map(_.x)
        val maxX = Xs.max
        val minX = Xs.min
        val Ys = lines.flatten.map(_.y)
        val maxY = Ys.max
        val minY = Ys.min
        val borderX = extension
        val borderY = 5
        Parameters(minX, maxX, minY, maxY, borderX, borderY)
    }

    private case class Parameters(minX: Int, maxX: Int, minY: Int, maxY: Int, borderX: Int, borderY: Int)
}
