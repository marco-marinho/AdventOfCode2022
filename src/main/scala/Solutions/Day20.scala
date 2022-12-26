package AoC2022
package Solutions

import Helpers.Readers

object Day20 {


  def main(args: Array[String]): Unit = {
    val data = Readers.readFile("Data/Day20.txt").map(_.toLong)
    val idxZero = data.indexOf(0)
    val nodest1 = parse(data, 1L)
    data.indices.foreach(it => move(it, nodest1))
    print("Task 01: ")
    println(nodest1(idxZero).get(1000).value + nodest1(idxZero).get(2000).value + nodest1(idxZero).get(3000).value)
    val nodes = parse(data, 811589153L)
    for (_ <- 0 until 10) {
      data.indices.foreach(it => move(it, nodes))
    }
    print("Task 02: ")
    println(nodes(idxZero).get(1000).value + nodes(idxZero).get(2000).value + nodes(idxZero).get(3000).value)
  }


  def move(nodeVal: Int, nodes: Map[Int, Node]): Unit = {
    val current = nodes(nodeVal)
    val target = current.get(current.value % (nodes.size - 1))
    if (current.value % (nodes.size - 1) == 0) return
    current.pop()
    current.putAfter(target)
  }

  def parse(nodes: List[Long], key: Long): Map[Int, Node] = {
    var nodeMap = Map[Int, Node]()
    val head = new Node(nodes.head * key)
    nodeMap += 0 -> head
    var current = head
    for (i <- 1 until nodes.size) {
      val next = new Node(nodes(i) * key)
      nodeMap += i -> next
      current.next = next
      next.previous = current
      current = next
    }
    head.previous = current
    current.next = head
    nodeMap
  }

  class Node(val value: Long) {
    var previous: Node = null
    var next: Node = null

    override def toString: String = value.toString + " [previous = " + previous.value + " | next = " + next.value + "]"

    def get(move: Long): Node = {
      var current = this
      for (_ <- 0L until move if move > 0) {
        current = current.next
      }
      for (_ <- 0L to -move if move < 0) {
        current = current.previous
      }
      current
    }

    def pop(): Unit = {
      this.previous.next = next
      this.next.previous = previous
    }

    def putAfter(other: Node): Unit = {
      this.previous = other
      this.next = other.next
      other.next.previous = this
      other.next = this
    }

  }
}
