package AoC2022
package Solutions

import Helpers.Readers
import Helpers.Utils.Point

object Day15 {
    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day15.txt")
        val sensors = parse(data)
        val searchArea = 4000000
        for (row <- 0 until searchArea) {
            val reachRow = sensors.filter(x => distToRow(x, row) <= x.distance)
            if (row == 2000000) {
                val rangesT1 = reachRow.map(x => getImpossible(x, row)).sortWith((a, b) => compareRange(a, b))
                val impossibleT1 = rangesT1.reduce((a, b) => a + b)
                println("Task 01: " + (impossibleT1.stop - impossibleT1.start))
            }
            val ranges = reachRow.map(x => getImpossible(x, row, 0, searchArea)).sortWith((a, b) => compareRange(a, b))
            val impossible = ranges.reduce((a, b) => a + b)
            if (impossible.stop - impossible.start == 0) {
                val hole = ranges.sliding(2).zipWithIndex.map(x => (x._1.head.intersect(x._1(1)), x._2)).filter(_._1 == false).toList
                val col = (ranges(hole.head._2).stop + ranges(hole.head._2 + 1).start) / 2
                println("Task 02: " + (col * 4000000L + row))
            }
        }
    }

    def parse(lines: List[String]): List[Pair] = {
        def parseLine(line: String): Pair = {
            line match {
                case s"Sensor at x=${xsens}, y=${ysens}: closest beacon is at x=${xbeac}, y=${ybeac}" =>
                    new Pair(ysens.toInt, xsens.toInt, ybeac.toInt, xbeac.toInt)
                case _ => throw new IllegalStateException("Could not parse input")
            }
        }

        lines.map(parseLine)
    }

    private def getImpossible(pair: Pair, row: Int, minValue: Int = Int.MinValue, maxValue: Int = Int.MaxValue): Range = {
        val baseDist = distToRow(pair, row)
        val maxDist = pair.distance
        val diff = maxDist - baseDist
        Range(Math.max(pair.sensor.y - diff, minValue), Math.min(pair.sensor.y + diff, maxValue))
    }

    private def compareRange(range1: Range, range2: Range): Boolean = {
        if (range1.start < range2.start) true
        else false
    }

    private def distToRow(pair: Pair, row: Int): Int = {
        pair.sensor.dist(Point(row, pair.sensor.y))
    }

    case class Pair(sensor: Point, beacon: Point, distance: Int) {
        def this(xs: Int, ys: Int, xb: Int, yb: Int) =
            this(Point(xs, ys), Point(xb, yb), Point(xs, ys).dist(Point(xb, yb)))
    }

    case class Range(start: Int, stop: Int) {
        def +(that: Range): Range = {
            if (!this.intersect(that)) Range(Int.MinValue, Int.MinValue)
            else Range(Math.min(this.start, that.start), Math.max(this.stop, that.stop))
        }

        def intersect(that: Range): Boolean = {
            (this.start <= that.stop + 1) && (this.stop + 1 >= that.start)
        }

    }

}