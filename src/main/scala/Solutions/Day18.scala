package AoC2022
package Solutions

import Helpers.Readers

import scala.collection.mutable

object Day18 {

  def main(args: Array[String]): Unit = {
    val data = Readers.readFile("Data/Day18.txt")
    val cubes = data.map(parse)

    var covered = 0
    for (i <- cubes.indices) {
      val current = cubes(i)
      val remaining = cubes.view.drop(i + 1)
      for (other <- remaining) {
        if (current.dist(other) == 1) covered += 2
      }
    }
    println("Task 01: " + ((cubes.size * 6) - covered))
    val bounds = getBoundBox(cubes)
    val res = floodFill(Cube(bounds._1, bounds._3, bounds._5), Set.from(cubes), bounds)
    println("Task 02: " + res)

  }

  private def floodFill(current: Cube, cubes: Set[Cube], bounds: (Int, Int, Int, Int, Int, Int)): Int = {
    var surface = 0
    var visited = Set[Cube]()
    val queue = mutable.Queue[Cube]()
    queue.addOne(current)
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      for (neighbour <- current.genNeighbours().filter(_.checkBounded(bounds))
           if !visited.contains(current)) {
        if (cubes.contains(neighbour)) surface += 1
        else if (!visited.contains(neighbour)) queue.addOne(neighbour)
      }
      visited += current
    }
    surface
  }


  private def getBoundBox(cubes: List[Cube]): (Int, Int, Int, Int, Int, Int) = {
    val xMin = cubes.minBy(_.x).x
    val xMax = cubes.maxBy(_.x).x
    val yMin = cubes.minBy(_.y).y
    val yMax = cubes.maxBy(_.y).y
    val zMin = cubes.minBy(_.z).z
    val zMax = cubes.maxBy(_.z).z
    (xMin - 1, xMax + 1, yMin - 1, yMax + 1, zMin - 1, zMax + 1)
  }

  def parse(line: String): Cube = {
    line match {
      case s"${x},${y},${z}" => Cube(x.toInt, y.toInt, z.toInt)
      case _ => throw new IllegalStateException("Could not parse line")
    }
  }

  case class Cube(x: Int, y: Int, z: Int) {
    def dist(other: Cube): Int = Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z)

    def checkBounded(bounds: (Int, Int, Int, Int, Int, Int)): Boolean = {
      val (xMin, xMax, yMin, yMax, zMin, zMax) = bounds
      x >= xMin && x <= xMax && y >= yMin && y <= yMax && z >= zMin && z <= zMax
    }

    def genNeighbours(): List[Cube] = List[Cube](
      Cube(x - 1, y, z), Cube(x + 1, y, z),
      Cube(x, y - 1, z), Cube(x, y + 1, z),
      Cube(x, y, z - 1), Cube(x, y, z + 1))
  }

}
