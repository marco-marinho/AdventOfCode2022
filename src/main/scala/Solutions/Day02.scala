package AoC2022
package Solutions

import Helpers.Readers

object Day02 {
  def main(args: Array[String]): Unit = {
    val data = Readers.readFile("Data/Day02.txt")
    val plays = data.map(it => Play(it.charAt(0), it.charAt(2)))
    println("Task 02: " + plays.map(_.score(false)).sum)
    println("Task 03: " + plays.map(_.score(true)).sum)
  }

  private case class Play(opponent: Char, you: Char) {
    def score(T2: Boolean): Int = {
      val you = if (T2) transformYourT2() else transformYour()
      val dif = opponent - you
      val res = dif match {
        case 0 => 3
        case 1 | -2 => 0
        case _ => 6
      }
      val play = you match {
        case 'A' => 1
        case 'B' => 2
        case 'C' => 3
      }
      res + play
    }

    private def transformYour(): Char = {
      you match {
        case 'X' => 'A'
        case 'Y' => 'B'
        case 'Z' => 'C'
      }
    }

    private def transformYourT2(): Char = {
      you match {
        case 'X' => if ((opponent - 'A') - 1 < 0) 'C' else (opponent - 1).toChar
        case 'Z' => ((((opponent - 'A') + 1) % 3) + 'A').toChar
        case 'Y' => opponent
      }
    }
  }
}

