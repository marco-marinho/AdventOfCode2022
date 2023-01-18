package AoC2022
package Helpers

import Helpers.Edge.Edge

import scala.reflect.ClassTag

class Cube[T:ClassTag](val size: Int, initialElement: T) {
  val faces = List.fill(6)(Matrix.fromElement(size, size, initialElement))
  var connections = Map(0 -> Map[Edge, Connection](), 1 -> Map[Edge, Connection](), 2 -> Map[Edge, Connection](),
    3 -> Map[Edge, Connection](), 4 -> Map[Edge, Connection](), 5 -> Map[Edge, Connection]())

  def setConnection(source: Int, dest: Int, edgeSource: Edge, destEdge: Edge, reverse: Boolean) = {
    val newConnection = connections(source) + (edgeSource -> Connection(dest, destEdge, reverse))
    connections = connections + (source -> newConnection)
  }

  def update(face: Int, row: Int, col: Int, value: T): Unit = faces(face)(row, col) = value
  def apply(face: Int, row: Int, col: Int):T =  faces(face)(row, col)
  def apply(face: Int): Matrix[T] = faces(face)
  case class Connection(face: Int, side: Edge, reverse: Boolean)
}

