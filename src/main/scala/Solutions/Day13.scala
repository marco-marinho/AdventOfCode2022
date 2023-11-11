package AoC2022
package Solutions

import Helpers.Readers

object Day13 {
    def main(args: Array[String]): Unit = {
        var data = Readers.readFile("Data/Day13.txt")
        val dividers = List[String]("[[2]]", "[[6]]").map(parseLine)
        data = data.filter(_.nonEmpty)
        val parsed = data.map(parseLine)
        val pairs = parsed.grouped(2).toList
        val res = pairs.map(x => compare(x.head, x(1)))
        val sorted = (dividers ::: parsed).sortWith((first, second) => compare(first, second) == 1)
        val t2 = dividers.map(x => sorted.map(_.toString).indexOf(x.toString) + 1).product
        val t1 = res.zipWithIndex.filter(_._1 == 1).map(_._2 + 1).sum
        println("Task 01: " + t1)
        println("Task 02: " + t2)
    }

    def compare(left: List[Any], right: List[Any]): Int = {
        val leftIt = left.iterator
        val rightIt = right.iterator
        while (leftIt.hasNext || rightIt.hasNext) {
            if (leftIt.hasNext && !rightIt.hasNext) return -1
            else if (!leftIt.hasNext && rightIt.hasNext) return 1
            else {
                val currLeft = leftIt.next()
                val currRight = rightIt.next()
                (currLeft, currRight) match {
                    case (lval: Int, rval: Int) =>
                        if (rval < lval) return -1
                        if (lval < rval) return 1
                    case (lval: List[Any], rval: Int) =>
                        if (compare(lval, List[Any](rval)) == -1) return -1
                        if (compare(lval, List[Any](rval)) == 1) return 1
                    case (lval: Int, rval: List[Any]) =>
                        if (compare(List[Any](lval), rval) == -1) return -1
                        if (compare(List[Any](lval), rval) == 1) return 1
                    case (lval: List[Any], rval: List[Any]) =>
                        if (compare(lval, rval) == -1) return -1
                        if (compare(lval, rval) == 1) return 1
                }
            }
        }
        0
    }

    def parseLine(input: String): List[Any] = {
        val line = input.drop(1).dropRight(1)
          .replace(",", " ").replace("[", " [ ")
          .replace("]", " ] ").replaceAll(" +", " ").trim
          .split(" ")
        var stack = List[List[Any]]()
        var current = List[Any]()
        for (element <- line) {
            element match {
                case "[" =>
                    stack = current :: stack
                    current = List[Any]()
                case "]" =>
                    current = current.reverse :: stack.head
                    stack = stack.drop(1)
                case "" =>
                case _ =>
                    val value = element.toInt
                    current = value :: current
            }
        }
        current.reverse
    }

}
