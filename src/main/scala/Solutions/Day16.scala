package AoC2022
package Solutions

import Helpers.Readers

import scala.collection.mutable

object Day16 {

  def main(args: Array[String]): Unit = {
    val data = Readers.readFile("Data/Day16.txt")
    val nodes = data.map(parse)
    val nodeMap = nodes.foldLeft(Map[String, Node]())((acc, curr) => acc + (curr.name -> curr))
    val allNodes = nodes.map(_.name)
    val targets = nodes.filter(_.flow > 0).map(_.name)
    var paths = allNodes.foldLeft(Map[(String, String), Path]())((acc, node) => {
      val connections = allNodes.map(neighbour => (node, neighbour) -> pathFind(node, neighbour, nodeMap))
      acc ++ connections
    })
    paths = paths.filter(_._2.dist > 0)

    val res1 = bfs(targets, paths, nodeMap, 30).maxBy(_.flown).flown

    val res = bfs(targets, paths, nodeMap, 26).sortBy(x => -x.flown)
    var res2 = 0
    var found = false
    for (leg <- res) {
      for (another <- res if !found) {
        if (Set.from(leg.visited).intersect(Set.from(another.visited)).isEmpty) {
          res2 = leg.flown + another.flown
          found = true
        }
      }
    }
    println("Task 01: " + res1)
    println("Task 02: " + res2)
  }

  private def bfs(targets: List[String], paths: Map[(String, String), Path], nodeMap: Map[String, Node], maxTime: Int): List[State] = {
    val stateQueue = mutable.Queue[State]()
    val bestState = mutable.Map[String, Int]()
    stateQueue.addOne(State("AA", List(), 0, 0))
    val solutions = mutable.ListBuffer[State]()
    while (stateQueue.nonEmpty) {
      val current = stateQueue.dequeue()
      val nextStates = current.genNextStates(targets, paths, nodeMap, maxTime)
      if (nextStates.isEmpty) {
        solutions.addOne(current)
      }
      else if (checkFeasible(current, bestState)) {
        for (state <- nextStates if checkFeasible(state, bestState)) {
          bestState.addOne(state.getRepr -> state.flown)
          stateQueue.addOne(state)
        }
      }
    }

    solutions.toList
  }

  private def checkFeasible(state: State, bestStates: mutable.Map[String, Int]): Boolean = {
    !bestStates.contains(state.getRepr) || bestStates(state.getRepr) <= state.flown
  }

  private def pathFind(start: String, end: String, nodes: Map[String, Node]): Path = {
    val toVisit = mutable.PriorityQueue[Path]()
    toVisit.addOne(Path(start, 0, List(start)))
    var visited = Set[String]()
    while (toVisit.nonEmpty) {
      val curr = toVisit.dequeue()
      if (visited.contains(curr.node)) {}
      else {
        val currDist = curr.dist
        visited += curr.node
        if (curr.node == end) return {
          curr
        }
        for (entry <- nodes(curr.node).connections) {
          if (!visited.contains(entry)) {
            toVisit.addOne(Path(entry, currDist + 1, entry :: curr.path))
          }
        }
      }
    }
    Path(start, Int.MaxValue, List[String]())
  }

  case class Path(node: String, dist: Int, path: List[String]) extends Ordered[Path] {
    def compare(that: Path): Int = that.dist compare this.dist
  }

  def parse(line: String): Node = {
    line match {
      case s"Valve ${valve} has flow rate=${flow}; tunnels lead to valves ${connections}" =>
        Node(valve, flow.toInt, connections.split(", "))
      case s"Valve ${valve} has flow rate=${flow}; tunnel leads to valve ${connection}" =>
        Node(valve, flow.toInt, Array[String](connection))
      case _ => throw new IllegalStateException("Cold not parse line")
    }
  }

  case class Node(name: String, flow: Int, connections: Array[String])

  private case class State(position: String, visited: List[String], flown: Int, minutes: Int) {
    def getRepr: String = position + ";" + visited.sorted.mkString(";")

    def genNextStates(targets: List[String], paths: Map[(String, String), Path], nodeMap: Map[String, Node], maxTime: Int): List[State] = {
      var states = List[State]()
      for (other <- targets) {
        if (other != this.position && !visited.contains(other)) {
          val nextVisited = other :: this.visited
          val distance = paths(this.position, other).dist + 1
          val nextDist = distance + this.minutes
          val newFlow = this.flown + (nodeMap(other).flow * (maxTime - nextDist))
          val nextState = State(other, nextVisited, newFlow, nextDist)
          if (nextDist <= maxTime) {
            states = nextState :: states
          }
        }
      }
      states
    }
  }

}
