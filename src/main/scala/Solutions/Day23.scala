package AoC2022
package Solutions

import Helpers.Readers

import scala.collection.mutable

object Day23 {

    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day23.txt")
        val elfs = data.zipWithIndex.flatMap(elfPos).toSet
        val state = new State(elfs)
        for (_ <- 0 until 10) {
            state.move()
        }
        println("Task 01: " + state.countEmpty())
        while (state.move()) {}
        println("Task 02: " + state.round)
    }

    private def elfPos(input: (String, Int)): List[Elf] = {
        input._1.zipWithIndex.filter(pair => pair._1 == '#').map(pair => Elf(input._2, pair._2)).toList
    }

    case class Elf(x: Int, y: Int) {

        def possibleMovements(elfs: Set[Elf]): Movement = {
            var north, west, east, south = true
            if (elfs.contains(Elf(x - 1, y))
              || elfs.contains(Elf(x - 1, y + 1))
              || elfs.contains(Elf(x - 1, y - 1))) north = false
            if (elfs.contains(Elf(x + 1, y))
              || elfs.contains(Elf(x + 1, y + 1))
              || elfs.contains(Elf(x + 1, y - 1))) south = false
            if (elfs.contains(Elf(x - 1, y - 1))
              || elfs.contains(Elf(x, y - 1))
              || elfs.contains(Elf(x + 1, y - 1))) west = false
            if (elfs.contains(Elf(x - 1, y + 1))
              || elfs.contains(Elf(x, y + 1))
              || elfs.contains(Elf(x + 1, y + 1))) east = false
            Movement(north, south, east, west)
        }

        def getMoved(cardinal: Int): Elf = {
            cardinal match {
                case 0 => Elf(x - 1, y)
                case 1 => Elf(x + 1, y)
                case 2 => Elf(x, y - 1)
                case 3 => Elf(x, y + 1)
                case _ => throw new IllegalArgumentException("Illegal cardinal.")
            }
        }

    }

    case class Movement(north: Boolean, south: Boolean, east: Boolean, west: Boolean) {
        def noNeighbours(): Boolean = north && south && east && west

        def cantMove(): Boolean = !north && !south && !east && !west

        def apply(cardinal: Int): Boolean = {
            cardinal match {
                case 0 => north
                case 1 => south
                case 2 => west
                case 3 => east
                case _ => throw new IllegalArgumentException("Illegal cardinal.")
            }
        }
    }

    private class State(var elfs: Set[Elf], var round: Int = 0) {
        private val priorities = List[Int](0, 1, 2, 3)

        def move(): Boolean = {
            var next = Set[Elf]()
            var proposals = List[(Elf, Elf)]()
            val proposal_count = mutable.Map[Elf, Int]()
            for (elf <- elfs) {
                val movements = elf.possibleMovements(elfs)
                if (movements.noNeighbours() || movements.cantMove()) {
                    next += elf
                }
                else {
                    var found = false
                    for (i <- round until round + 4 if !found) {
                        val movement = priorities(i % 4)
                        if (movements(movement)) {
                            val movedElf = elf.getMoved(movement)
                            proposals = (elf, movedElf) :: proposals
                            proposal_count(movedElf) = 1 + proposal_count.getOrElse(movedElf, 0)
                            found = true
                        }
                    }
                }
            }
            if (proposals.isEmpty) {
                round += 1
                return false
            }
            for (proposal <- proposals) {
                if (proposal_count(proposal._2) == 1) next += proposal._2
                else next += proposal._1
            }
            round += 1
            elfs = next
            true
        }

        def countEmpty(): Int = {
            val xs = elfs.map(_.x)
            val ys = elfs.map(_.y)
            val x_min = xs.min
            val x_max = xs.max
            val y_min = ys.min
            val y_max = ys.max
            val tiles = (x_max - x_min + 1) * (y_max - y_min + 1)
            tiles - elfs.size
        }

    }

}
