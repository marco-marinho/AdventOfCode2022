package AoC2022
package Helpers

import scala.collection.mutable.ListBuffer

object Utils {
  def splitAt[T](data: List[T])(pred: T => Boolean): List[List[T]] = {
    val chunks = data.zipWithIndex.filter(x => pred(x._1)).map(_._2)
    var start = -1
    val buffer = new ListBuffer[List[T]]()
    for (stop <- chunks) {
      buffer += data.slice(start + 1, stop)
      start = stop
    }
    buffer += data.slice(start + 1, data.size)
    buffer.toList
  }

  case class Point(x: Int, y: Int) {
    def this(coords: (Int, Int)) = this(coords._1, coords._2)
    def +(other: (Int, Int)): Point = Point(x + other._1, y + other._2)

    def +(other: Point): Point = Point(x + other.x, y + other.y)

    def dist(other: Point): Int = Math.abs(x - other.x) + Math.abs(y - other.y)
  }

}