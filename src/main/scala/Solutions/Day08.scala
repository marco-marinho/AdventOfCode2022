package AoC2022
package Solutions

import Helpers.Readers
import Helpers.Utils.Point

import scala.annotation.tailrec

object Day08 {
    type Matrix = List[List[Int]]

    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day08.txt")
        val matrix = data.map(_.toCharArray.map(_.getNumericValue).toList)
        val nRows = matrix.size
        val nCols = matrix.head.size
        val visible =
            for (i <- 1 until nRows - 1; j <- 1 until nCols - 1) yield {
                val row = getHorNeigh(Point(i, j), matrix)
                val col = getVerNeigh(Point(i, j), matrix)
                val value = matrix(i)(j)
                val visRow = row.map(_.forall(_ < value)).count(_ == true)
                val visCol = col.map(_.forall(_ < value)).count(_ == true)
                val rangeRow = row.map(countView(_, value)).product
                val rangeCol = col.map(countView(_, value)).product
                (visRow + visCol, rangeRow * rangeCol)
            }
        println("Task 01: " + (visible.map(_._1).count(_ > 0) + nRows * 2 + nCols * 2 - 4))
        println("Task 02: " + visible.map(_._2).max)
    }

    @tailrec
    private def countView(it: List[Int], value: Int, current: Int = 0): Int = {
        val currHead = it.headOption
        currHead match {
            case Some(x) =>
                if (x < value) countView(it.tail, value, current + 1) else current + 1
            case None => current
        }
    }

    private def getVerNeigh(coords: Point, matrix: Matrix): Matrix = {
        val pieces = getCol(coords.y, matrix).splitAt(coords.x)
        List(pieces._1.reverse, pieces._2.drop(1))
    }

    private def getHorNeigh(coords: Point, matrix: Matrix): Matrix = {
        val pieces = getRow(coords.x, matrix).splitAt(coords.y)
        List(pieces._1.reverse, pieces._2.drop(1))
    }

    private def getRow(row: Int, matrix: Matrix): List[Int] = {
        matrix(row)
    }

    private def getCol(col: Int, matrix: Matrix): List[Int] = {
        matrix.map(_(col))
    }

}
