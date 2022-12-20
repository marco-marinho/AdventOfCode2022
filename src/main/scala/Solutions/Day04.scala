package AoC2022
package Solutions

import Helpers.Readers

object Day04 {
  def main(args: Array[String]): Unit = {
    val data = Readers.readFile("Data/Day04.txt")
    val parsed = data.map(_.replace(",", "-").split("-").map(_.toInt).toList).map(new Limits(_))
    println("Task 01: " + parsed.map(_.overlap()).count(_ == true))
    println("Task 02: " + parsed.map(_.overlapAny()).count(_ == true))
  }

  case class Limits(min1: Int, max1: Int, min2: Int, max2: Int){
    def this(entry: List[Int]) = this(entry.head,entry(1), entry(2), entry(3))
    def overlap() : Boolean = {
      (min1 >= min2 && max1 <= max2) || (min2 >= min1 && max2 <= max1)
    }
    def overlapAny() : Boolean = {
      (max1 >= min2 && max1 <= max2) || (max2 >= min1 && max2 <= max1)
    }
  }
}
