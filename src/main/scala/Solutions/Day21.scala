package AoC2022
package Solutions

import Helpers.Readers

object Day21 {

    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day21.txt")
        var elements = Map[String, Element]()
        data.foreach(it => {
            val res = parse(it)
            elements += res._1 -> res._2
        })
        println("Task 01: " + resolve(elements("root"), elements).toLong)
        val target = resolve(elements("rqsg"), elements)
        val reversed = calc(elements, "wgbd", 0) < calc(elements, "wgbd", 100)
        val res = binarySearch(elements, "wgbd", target, reversed).toLong
        println("Task 02: " + res)
    }

    def parse(line: String): (String, Element) = {
        line match {
            case s"${name}: ${operand1} ${operation} ${operand2}" => (name, Element(name, None, Some(operand1, operand2, operation)))
            case s"${name}: ${value}" => (name, Element(name, Some(value.toInt), None))
        }
    }

    private def binarySearch(elements: Map[String, Element], branchName: String,
                             target: Double, reversed: Boolean): Double = {
        var lower = 0.0
        var upper = 1e16
        var half = (upper + lower) / 2
        var current = calc(elements, branchName, half)
        while (current != target) {

            if (current > target) {
                if (reversed) {
                    upper = half
                } else {
                    lower = half
                }
            }
            if (current < target) {
                if (reversed) {
                    lower = half
                } else {
                    upper = half
                }
            }
            half = (upper + lower) / 2
            current = calc(elements, branchName, half)
        }
        half
    }

    private def calc(elements: Map[String, Element], name: String, value: Double): Double = {
        val map = elements + ("humn" -> Element("humn", Some(value), None))
        resolve(map(name), map)
    }

    private def resolve(element: Element, elements: Map[String, Element]): Double = {
        element.value match {
            case Some(value) => value
            case None => calculate(element, elements)
        }
    }

    private def calculate(element: Element, elements: Map[String, Element]): Double = {
        element.operation match {
            case None => throw new IllegalStateException("No information to calculate element")
            case Some(operation) =>
                val (first, second, op) = operation
                val firstEl = resolve(elements(first), elements)
                val secondEl = resolve(elements(second), elements)
                op match {
                    case "+" => firstEl + secondEl
                    case "*" => firstEl * secondEl
                    case "-" => firstEl - secondEl
                    case "/" => firstEl / secondEl
                    case _ => throw new IllegalStateException("Invalid operation")
                }
        }
    }

    case class Element(name: String, value: Option[Double], operation: Option[(String, String, String)])

}
