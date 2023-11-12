package AoC2022
package Solutions

import Helpers.{Readers, Utils}

object Day01 {
    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day01.txt")
        val packs = Utils.splitAt(data)(_ == "").map(_.map(_.toInt))
        println("Task 01: " + packs.map(_.sum).max)
        println("Task 02: " + packs.map(_.sum).sorted.reverse.take(3).sum)
    }
}