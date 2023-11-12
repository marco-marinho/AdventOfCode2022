package AoC2022
package Solutions

import Helpers.Readers

import scala.collection.mutable

object Day24 {
    def main(args: Array[String]): Unit = {

        val data = Readers.readFile("Data/Day24.txt")
        val x_max = data.size - 2
        val y_max = data.head.length - 2
        val blizzard = data.zipWithIndex.flatMap(blizzardPos)
        val blizzards = mutable.Map[Int, List[Blizzard]](0 -> blizzard)
        val blizzardSets = mutable.Map[Int, Set[(Int, Int)]](0 -> blizzard.map(pair => (pair.x, pair.y)).toSet)
        var res = dfs((0, 0, 0), blizzards, blizzardSets, x_max, y_max, (x_max + 1, y_max))
        println("Task 01: " + res._3)
        res = dfs(res, blizzards, blizzardSets, x_max, y_max, (0, 1))
        res = dfs(res, blizzards, blizzardSets, x_max, y_max, (x_max + 1, y_max))
        println("Task 02: " + res._3)
    }

    private def blizzardPos(input: (String, Int)): List[Blizzard] = {
        val isBlizzard = (c: Char) => c == '>' || c == '<' || c == '^' || c == 'v'
        input._1
          .toList
          .zipWithIndex
          .filter(pair => isBlizzard(pair._1))
          .map(pair => Blizzard(input._2, pair._2, pair._1))
    }

    def dfs(initialState: (Int, Int, Int), blizzards: mutable.Map[Int, List[Blizzard]],
            blizzardSets: mutable.Map[Int, Set[(Int, Int)]],
            x_max: Int, y_max: Int, target: (Int, Int)): (Int, Int, Int) = {
        val seen = mutable.Set[(Int, Int, Int)]()
        val toCheck = mutable.Queue[(Int, Int, Int)](initialState)
        val steps = Array[(Int, Int)]((1, 0), (-1, 0), (0, 1), (0, -1), (0, 0))
        while (toCheck.nonEmpty) {
            val (x, y, turn) = toCheck.dequeue()
            if ((x, y) == target) return (x, y, turn)
            var proceed = false
            if (!seen.contains((x, y, turn))) {
                seen.add((x, y, turn))
                proceed = true
            }
            if (proceed) {
                if (!blizzards.contains(turn + 1)) {
                    blizzards += (turn + 1 -> blizzards(turn).map(_.walk(x_max, y_max)))
                    blizzardSets += (turn + 1 -> blizzards(turn + 1).map(_.toTuple).toSet)
                }
                val next_blizzards = blizzardSets(turn + 1)
                for (step <- steps) {
                    val next = (x + step._1, y + step._2)
                    if (!next_blizzards.contains(next) && stepValid(next, x_max, y_max)) {
                        toCheck.enqueue((next._1, next._2, turn + 1))
                    }
                }
            }
        }
        (-1, -1, -1)
    }

    private def stepValid(step: (Int, Int), x_max: Int, y_max: Int): Boolean = {
        val (x, y) = step
        if (x == 0 && y == 1) true
        else if (x == x_max + 1 && y == y_max) true
        else if (x < 1 || x > x_max || y < 1 || y > y_max) false
        else true
    }

    case class Blizzard(x: Int, y: Int, direction: Char) {

        def toTuple: (Int, Int) = {
            (x, y)
        }

        def walk(x_max: Int, y_max: Int): Blizzard = {
            direction match {
                case '^' => Blizzard(if (x - 1 >= 1) x - 1 else x_max, y, direction)
                case 'v' => Blizzard(if (x + 1 <= x_max) x + 1 else 1, y, direction)
                case '<' => Blizzard(x, if (y - 1 >= 1) y - 1 else y_max, direction)
                case '>' => Blizzard(x, if (y + 1 <= y_max) y + 1 else 1, direction)
            }
        }

    }

}
