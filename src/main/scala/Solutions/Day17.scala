package AoC2022
package Solutions

import Helpers.Readers
import Helpers.Utils.Point

object Day17 {

  def main(args: Array[String]): Unit = {
    println("Task 01: " + runSimulation())
    println("Taks 02: " + runSimulation(true))
  }

  private def runSimulation(task02: Boolean = false): Long = {
    val data = Readers.readFile("Data/Day17.txt").head.toCharArray
    val dropper = new pieceDropper
    var windIdx = 0
    var board = Set[Point](Point(0, 1), Point(0, 2), Point(0, 3), Point(0, 4), Point(0, 5), Point(0, 6), Point(0, 7))
    var pieces = 0
    var states = Map[String, (Int, Int)]()
    var found = false
    var added = 0L
    val max = if (task02) 10000 else 2022
    while (pieces < max) {
      var curPiece = dropper.genPiece(getTallest(board))
      var settled = false
      if (!found && task02) {
        val (state, heigth) = rowMax(board)
        val idxstr = dropper.getIdx()
        val key = windIdx.toString + ";" + state.mkString(";") + ";" + idxstr
        val value = (pieces, heigth)
        if (!states.contains(key)) states += (key -> value)
        else {
          val (opieces, oheight) = states(key)
          val diff = pieces - opieces
          val diffhei = heigth - oheight
          added = ((1000000000000L - pieces) / diff) * diffhei
          val remaining = ((1000000000000L - pieces) % diff)
          pieces = (max - remaining).toInt
          found = true
        }
      }
      while (!settled) {
        windIdx %= data.length
        val wind = data(windIdx)
        val (pieceBuff, status) = movePiece(curPiece, wind, board)
        curPiece = pieceBuff
        settled = status
        windIdx += 1
      }
      board = board ++ curPiece
      pieces += 1
    }
    board.maxBy(_.x).x + added
  }

  private def rowMax(board: Set[Point]): (List[Int], Int) = {
    val top = getTallest(board)
    var res = List[Int]()
    for (i <- 1 until 8) {
      res = (board.filter(_.y == i).maxBy(_.x).x - top) :: res
    }
    (res, top)
  }
  private def printBoard(board: Set[Point]) = {
    var currRow = board.maxBy(_.x).x
    while (currRow >= 1) {
      val row = board.filter(_.x == currRow)
      val rowPrint = Array.fill(9)('.')
      rowPrint(0) = '|'
      rowPrint(8) = '|'
      row.foreach(x => rowPrint(x.y) = '#')
      println(rowPrint.mkString(""))
      currRow-=1
    }
    println("+-------+")
  }

  private def movePiece(piece: Set[Point], direction: Char, board: Set[Point]): (Set[Point], Boolean) = {
    val (leftLim, bottomLim, rightLim) = getBorders(piece)
    val moved = (leftLim, bottomLim, rightLim) match {
      case (1, _, _) if direction == '<' => piece
      case (_, _, 7) if direction == '>' => piece
      case (_, _, _) if move(piece, direction).exists(board.contains) => piece
      case _ => move(piece, direction)
    }
    move(moved, 'v') match {
      case dropped if !dropped.exists(board.contains) => (dropped, false)
      case _ => (moved, true)
    }
  }

  def move(piece: Set[Point], direction: Char): Set[Point] = {
    direction match {
      case '<' => piece.map(buff => Point(buff.x, buff.y - 1))
      case '>' => piece.map(buff => Point(buff.x, buff.y + 1))
      case 'v' => piece.map(buff => Point(buff.x - 1, buff.y))
      case '_' => throw new IllegalStateException("Illegal command")
    }
  }

  private def getBorders(piece: Set[Point]): (Int, Int, Int) = {
    (piece.minBy(_.y).y, piece.minBy(_.x).x, piece.maxBy(_.y).y)
  }

  private def getTallest(points: Set[Point]): Int = {
    if (points.isEmpty) 1
    else points.maxBy(_.x).x
  }

  private class pieceDropper() {
    var idx = 0

    def getIdx():String = (idx%5).toString
    def genPiece(top: Int): Set[Point] = {
      val x = top + 4
      idx += 1
      (idx - 1) % 5 match {
        case 0 =>
          Set[Point](Point(x, 3), Point(x, 4), Point(x, 5), Point(x, 6))
        case 1 =>
          Set[Point](Point(x, 4), Point(x + 1, 3), Point(x + 1, 4), Point(x + 1, 5), Point(x + 2, 4))
        case 2 =>
          Set[Point](Point(x, 3), Point(x, 4), Point(x, 5), Point(x + 1, 5), Point(x + 2, 5))
        case 3 =>
          Set[Point](Point(x, 3), Point(x + 1, 3), Point(x + 2, 3), Point(x + 3, 3))
        case 4 =>
          Set[Point](Point(x, 3), Point(x, 4), Point(x + 1, 3), Point(x + 1, 4))
      }
    }

  }

}
