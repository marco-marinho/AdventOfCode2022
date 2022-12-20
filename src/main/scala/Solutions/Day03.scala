package AoC2022
package Solutions

import Helpers.Readers

object Day03 {
  def main(args: Array[String]): Unit = {
    val data = Readers.readFile("Data/Day03.txt")
    val halves = data.map(it => it.splitAt(it.length/2))
    println("Task 01: " + halves.map(getCommon).map(score).sum)
    println("Task 02: " + data.grouped(3).map(getCommonTrio).map(score).sum)
  }

  private def getCommon(halves: (String, String)): Char = {
    Set.from(halves._1.toCharArray).intersect(Set.from(halves._2.toCharArray)).head
  }

  private def getCommonTrio(trio: List[String]): Char = {
    var now = Set.from(trio.head)
    for (entry <- trio) {
      now = now.intersect(Set.from(entry))
    }
    now.head
  }

  private def score(input: Char) = {
    if (input >= 'a' && input <= 'z'){
      input - 'a' + 1
    }else{
      input - 'A' + 27
    }
  }

}
