package AoC2022
package Solutions

import Helpers.Readers

object Day25 {
    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day25.txt")
        val decimals = data.map(toDecimal)
        println("Task 01: " + toSnafu(decimals.sum))
    }

    private def toDecimal(s: String): Long = {
        s
          .toList
          .reverse
          .zipWithIndex
          .map(pair => (math.pow(5, pair._2) * multiplier(pair._1)).toLong)
          .sum
    }

    private def multiplier(c: Char): Int = {
        c match {
            case '2' => 2
            case '1' => 1
            case '0' => 0
            case '-' => -1
            case '=' => -2
        }
    }

    private def toSnafu(n: Long): String = {
        val digits = List[Char]('=', '-', '0', '1', '2')
        var snafu = List[Char]()
        var num = n
        while (num != 0) {
            val index = (num + 2) % 5
            snafu = digits(index.toInt) :: snafu
            if (index < 2) {
                num += 5
            }
            num /= 5
        }
        snafu.mkString("")
    }

}
