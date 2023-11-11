package AoC2022
package Solutions

import Helpers.Readers

object Day10 {
    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day10.txt")
        val operation = parse(data)
        val cycles = operation.foldLeft(List[Long](1))((acc, curr) =>
            curr match {
                case Operation(OpType.Add, value) => acc.head + value :: acc.head :: acc
                case _ => acc.head :: acc
            }
        ).reverse
        var sum = 0L
        for (i <- List[Int](20, 60, 100, 140, 180, 220)) {
            sum += i * cycles(i - 1)
        }
        println("Task 01: " + sum)

        val pixels = cycles.zipWithIndex.map(x =>
            if (Math.abs(x._1 - x._2 % 40) <= 1) "##"
            else ".."
        ).grouped(40).toList.dropRight(1)

        println("Task 02:")
        pixels.foreach(line =>
            println(line.mkString("")))
    }

    def parse(data: List[String]): List[Operation] = {

        def parseLine(input: String): Operation = {
            input match {
                case s"addx $value" => Operation(OpType.Add, value.toLong)
                case _ => Operation(OpType.Nop, 0)
            }
        }

        data.map(parseLine)
    }

    sealed trait OpType

    case class Operation(op: OpType, value: Long) {}

    object OpType {
        case object Nop extends OpType

        case object Add extends OpType
    }
}
