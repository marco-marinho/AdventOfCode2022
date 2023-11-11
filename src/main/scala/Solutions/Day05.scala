package AoC2022
package Solutions

import Helpers.{Readers, Utils}

import scala.collection.mutable.ListBuffer

object Day05 {
    def main(args: Array[String]): Unit = {
        val (stack, commands) = getStack
        runCommands(stack, commands, T2 = false)
        var t1 = ""
        for (col <- stack) {
            t1 += col.head
        }
        println("Task 01: " + t1)
        val (stack2, _) = getStack
        runCommands(stack2, commands, T2 = true)
        var t2 = ""
        for (col <- stack2) {
            t2 += col.head
        }
        println("Task 02: " + t2)
    }

    private def getStack: (ListBuffer[ListBuffer[Char]], List[command]) = {
        val data = Readers.readFile("Data/Day05.txt")
        val split = Utils.splitAt(data)((x: String) => x == "")
        val initial = split.head.dropRight(1).reverse
        val order = initial.map(blocks)
        var stack = ListBuffer.fill(order.head.length)(ListBuffer[Char]())
        updateStack(stack, order)
        stack = stack.map(_.reverse)
        val commands = split(1).map(it => it.replace("move ", "").replace(" from ", " ")
          .replace(" to ", " ").split(" ").map(_.toInt)).map(it => command(it.head, it(1) - 1, it(2) - 1))
        (stack, commands)
    }

    private def updateStack(stack: ListBuffer[ListBuffer[Char]], input: List[List[Char]]): Unit = {
        for (row <- input) {
            for (entry <- row.zipWithIndex) {
                if (entry._1 != ' ') {
                    stack(entry._2) += entry._1
                }
            }
        }
    }

    private def blocks(input: String): List[Char] = {
        input.toList.drop(1).grouped(4).map(_.head).toList
    }

    private def runCommands(stack: ListBuffer[ListBuffer[Char]], commands: List[command], T2: Boolean): Unit = {
        for (command <- commands) {
            var taken = stack(command.origin).take(command.num)
            if (!T2) taken = taken.reverse
            stack(command.origin).dropInPlace(command.num)
            stack(command.dest).insertAll(0, taken)
        }
    }

    case class command(num: Int, origin: Int, dest: Int) {
    }

}
