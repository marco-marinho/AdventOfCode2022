package AoC2022
package Solutions

import Helpers.Readers

object Day06 {
  def main(args: Array[String]): Unit = {
    val data = Readers.readFile("Data/Day06.txt").head
    val calc = (len: Int) => LazyList.from(data).sliding(len).takeWhile(it => Set.from(it).size < len).size + len
    println("Task 01: " + calc(4))
    println("Task 02: " + calc(14))
  }
}
