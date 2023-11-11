package AoC2022
package Solutions

import Helpers.Readers
import Helpers.Utils.Point

object Day09 {
    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day09.txt")
        val steps = parse(data)
        val headPositions = steps.scanLeft(Point(0, 0))(_ + _)
        var tailPositions = calcTails(headPositions)
        println("Task 01: " + Set.from(tailPositions).size)
        for (_ <- 1 until 9) {
            tailPositions = calcTails(tailPositions)
        }
        println("Task 02: " + Set.from(tailPositions).size)
    }

    private def calcTails(heads: List[Point]): List[Point] = {
        heads.foldLeft(List(Point(0, 0)))((acc, headPos) =>
            tailWalk(headPos, acc.head) :: acc
        ).reverse.drop(1)
    }

    private def tailWalk(head: Point, tail: Point): Point = {
        val diffX = tail.x - head.x
        val diffY = tail.y - head.y
        val dist = head.dist(tail)
        (diffX, diffY, dist) match {
            case (diffX, 0, dist) if dist >= 2 => tail + (-diffX / 2, 0)
            case (0, diffY, dist) if dist >= 2 => tail + (0, -diffY / 2)
            case (diffX, diffY, dist) if dist > 2 => tail + (-diffX.sign, -diffY.sign)
            case _ => tail
        }
    }

    private def parse(data: List[String]): List[Point] = {
        def parseLine(line: String): List[Point] = {
            val pieces = line.split(" ")
            val times = pieces(1).toInt
            pieces.head match {
                case "R" => List.fill(times)(Point(0, 1))
                case "L" => List.fill(times)(Point(0, -1))
                case "U" => List.fill(times)(Point(1, 0))
                case "D" => List.fill(times)(Point(-1, 0))
            }
        }

        data.flatMap(parseLine)
    }
}
