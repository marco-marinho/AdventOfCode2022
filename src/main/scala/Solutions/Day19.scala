package AoC2022
package Solutions

import Helpers.Readers

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

object Day19 {

    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day19.txt")
        val recipes = data.map(parse)
        println("Task 01: " + recipes.zipWithIndex.par.map(it => simulate(it._1, 24) * it._1.idx).sum)
        println("Task 02: " + recipes.take(3).par.map(simulate(_, 32)).product)
    }

    private def simulate(recipe: Recipe, maxTurns: Int): Int = {
        val alpha = initialState
        val queue = mutable.Queue[State]()
        queue.addOne(alpha)
        var seen = Set[State]()
        var max = 0
        while (queue.nonEmpty) {
            val current = queue.dequeue()
            val nextStates = current.genNextStates(recipe, maxTurns)
            if (nextStates.isEmpty) {
                val solution = current.resources("geode") + ((maxTurns - current.turns) * current.rates("geode"))
                if (solution > max) max = solution
            }
            else if (checkFeasible(current, maxTurns, max) && !seen.contains(current)) {
                seen += current
                for (state <- nextStates if checkFeasible(state, maxTurns, max)) {
                    queue.addOne(state)
                }
            }
        }
        max
    }

    def checkFeasible(state: State, maxTurns: Int, max: Int): Boolean = {
        val turnsLeft = maxTurns - state.turns
        max <= state.resources("geode") + (turnsLeft * state.rates("geode")) + (turnsLeft * turnsLeft) / 2
    }

    private def initialState = State(0, resourceDict,
        resourceDict + ("ore" -> 1))

    private def resourceDict: Map[String, Int] = Map[String, Int]("ore" -> 0, "clay" -> 0, "obsidian" -> 0, "geode" -> 0)

    def parse(line: String): Recipe = {
        val args = ("""\d+""".r findAllIn line).toList.map(_.toInt)
        val oreCosts = resourceDict + ("ore" -> args(1))
        val clayCosts = resourceDict + ("ore" -> args(2))
        val obsidianCosts = resourceDict ++ Map("ore" -> args(3), "clay" -> args(4))
        val geodeCosts = resourceDict ++ Map("ore" -> args(5), "obsidian" -> args(6))
        Recipe(args.head, Map("ore" -> oreCosts, "clay" -> clayCosts, "obsidian" -> obsidianCosts, "geode" -> geodeCosts))
    }

    private def checkPossible(cost: Map[String, Int], rate: Map[String, Int]): Boolean = {
        cost.filter(it => it._2 > 0).forall(it => rate(it._1) > 0)
    }

    private def calcTurnsNeeded(rate: Int, current: Int, needed: Int): Int = {
        val diff = needed - current
        if (diff < 0) 0
        else if (needed > 0 && rate == 0) 100000
        else if (current == needed) 0
        else {
            val diff = needed - current
            val turnsNeeded = (diff - 1) / rate + 1
            turnsNeeded
        }
    }

    case class Recipe(idx: Int, costs: Map[String, Map[String, Int]]) {
        val maxCosts: Map[String, Int] = Map("ore" -> costs.map(_._2("ore")).max, "clay" -> costs("obsidian")("clay"),
            "obsidian" -> costs("geode")("obsidian"), "geode" -> Int.MaxValue)
    }

    case class State(turns: Int, resources: Map[String, Int], rates: Map[String, Int]) {

        def genNextStates(recipe: Recipe, maxTurns: Int): List[State] = {
            var nextStates = List[State]()
            for (resource <- recipe.costs.keys if (rates(resource) < recipe.maxCosts(resource)
              && checkPossible(recipe.costs(resource), rates))) {
                val costs = recipe.costs(resource)
                val turnsNeeded = costs.map(entry => calcTurnsNeeded(rates(entry._1), resources(entry._1), entry._2)).max
                if (turnsNeeded >= 0 && turnsNeeded + turns < maxTurns) {
                    val nextRates = rates + (resource -> (rates(resource) + 1))
                    val nextResources = resources.map(it => it._1 -> (it._2 + ((turnsNeeded + 1) * rates(it._1)) - costs(it._1)))
                    nextStates = State(turns + turnsNeeded + 1, nextResources, nextRates) :: nextStates
                }
            }
            nextStates
        }
    }
}
