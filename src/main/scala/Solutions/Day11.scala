package AoC2022
package Solutions

import Helpers.Readers
import Helpers.Utils

import scala.collection.mutable.ListBuffer

object Day11 {
  def main(args: Array[String]): Unit = {
    println("Task 01: " + runMonkeyBusiness(20, 3, factor = false))
    println("Task 02: " + runMonkeyBusiness(10000, 1, factor = true))
  }

  private def runMonkeyBusiness(iterations: Int, divisor: Long, factor: Boolean): Long = {
    val data = Readers.readFile("Data/Day11.txt")
    val split = Utils.splitAt(data)(x => x == "")
    val parsed = split.map(parse)
    val monkeys = parsed.map(_._2)
    val commonFactor = if (!factor) 1L else monkeys.map(_.modulo).product
    var curState = parsed.map(_._1)
    val throws = Array.fill(parsed.size)(0L)
    for (_ <- 0 until iterations) {
      val nextState = List.fill(parsed.size)(ListBuffer[Long]())
      curState.zip(monkeys).foreach(data => {
        val (items, monkey) = data
        val targetVals = (items ::: nextState(monkey.index).toList).map(monkey.getTargetNewValue(_, divisor, commonFactor))
        throws(monkey.index) += targetVals.size
        targetVals.foreach(target => {
          val (dest, item) = target
          nextState(dest) += item
          nextState(monkey.index).clear()
        })
      }
      )
      curState = nextState.map(_.toList)
    }
    throws.sorted.takeRight(2).product
  }

  def parse(lines: List[String]): (List[Long], Monkey) = {
    val index = lines.head.split(" ").last.replace(":", "").toInt
    val items = lines(1).trim match {
      case s"Starting items: $items" => items.split(", ").map(_.toLong)
      case _ => throw new IllegalArgumentException("Cannot parse starting items")
    }
    val operation = lines(2) match {
      case sqr if sqr.contains("old * old") => Operation.Sqr
      case add if add.contains("+") => Operation.Add
      case mul if mul.contains("*") => Operation.Mul
      case _ => throw new IllegalArgumentException("Cannot parse starting operation")
    }
    val operand = operation match {
      case Operation.Sqr => 0
      case _ => lines(2).trim.split(" ").last.toInt
    }
    val modulo = lines(3).trim.split(" ").last.toInt
    val targetTrue = lines(4).trim.split(" ").last.toInt
    val targetFalse = lines(5).trim.split(" ").last.toInt
    (items.toList, Monkey(index, operation, operand, modulo, targetTrue, targetFalse))
  }


  case class Monkey(index: Int, operation: Operation, operand: Long, modulo: Long, targetTrue: Int, targetFalse: Int) {
    def getTargetNewValue(value: Long, divisor: Long, commonFactor: Long = 1): (Int, Long) = {
      var newValue = operation match {
        case Operation.Sqr => (value * value) / divisor
        case Operation.Add => (value + operand) / divisor
        case Operation.Mul => (value * operand) / divisor
      }
      val target = if (newValue % modulo == 0) {
        targetTrue
      } else {
        targetFalse
      }
      if (commonFactor != 1) newValue = newValue % commonFactor
      (target, newValue)
    }
  }

  sealed trait Operation

  object Operation {
    case object Mul extends Operation

    case object Add extends Operation

    case object Sqr extends Operation
  }
}
