package AoC2022
package Helpers

import scala.reflect.ClassTag

class Matrix[T:ClassTag](input: Array[Array[T]]) {

  val data: Array[Array[T]] = input.map(_.clone)
  val nrows: Int = data.length
  val ncols: Int = data(0).length
  def apply(row: Int, col: Int):T = data(row)(col)
  def update(row: Int, col: Int, value: T): Unit = data(row)(col) = value
  def find(element: T): (Int, Int) = {
    val search = data.map(_.indexOf(element))
    val row = search.indexWhere(_ != -1)
    val col = search.filter(_ != -1).head
    (row, col)
  }

  def findAll(element: T): List[(Int, Int)] = {
    var output = List[(Int, Int)]()
    for (i <- 0 until nrows){
      val cols = data(i).zipWithIndex.filter(_._1 == element).map(_._2)
      output = cols.map((i, _)).toList ::: output
    }
    output
  }

  def countAll(element:T): Int = {
    findAll(element).size
  }

  def neighboursCoords(row: Int, col: Int): List[(Int, Int)] = {
    val neighs = List[(Int, Int)]((row+1, col), (row-1, col), (row, col+1), (row, col-1))
    val res = neighs.filter(x => x._1 >= 0 && x._1 < nrows && x._2 >= 0 && x._2 < ncols)
    res
  }

  def neighbours(row: Int, col: Int): List[T] = {
    val coords = neighboursCoords(row, col)
    coords.map(x => data(x._1)(x._2))
  }

  def print(): Unit = {
    data.foreach(x => println(x.mkString("")))
  }

  override def toString: String = {
    data.map(x => x.mkString("")).mkString("\n")
  }

}

object Matrix {
  def fromElement[T: ClassTag](nrows: Int, ncols: Int, element: T): Matrix[T] = {
    val data = Array.fill(nrows)(Array.fill(ncols)(element))
    new Matrix(data)
  }
}
