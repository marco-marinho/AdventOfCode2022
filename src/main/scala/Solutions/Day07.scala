package AoC2022
package Solutions

import Helpers.Readers

object Day07 {
    def main(args: Array[String]): Unit = {
        val data = Readers.readFile("Data/Day07.txt")
        val nodes = processInput(data)
        val sizes = nodes.keys.toList.map(el => calcSize(nodes(el)))
        println("Task 01: " + sizes.filter(_ <= 100000).sum)
        val needed = 30000000 - (70000000 - calcSize(nodes("/")))
        println("Task 02: " + sizes.filter(_ >= needed).min)
    }

    private def calcSize(node: FS): Long = {
        val size = node.getSize + node.getChildren.map(calcSize).sum
        size
    }

    private def processInput(input: List[String]): Map[String, FS] = {
        val root = new FS("/")
        var res = Map("/" -> root)
        var curName = "/"
        var current = root
        var size = 0L
        for (line <- input.drop(1)) {
            val input = line match {
                case back if back.equals("$ cd ..") => false
                case cd if cd.startsWith("$ cd ") => cd.drop(5)
                case ls if ls.equals("$ ls") => null
                case dir if dir.startsWith("dir") => null
                case other => other.split(" ")(0).toLong
            }
            input match {
                case value: Long => size += value

                case name: String =>
                    curName = curName + name + "/"
                    val next = new FS(curName)
                    res += (curName -> next)
                    next.updateParent(current)
                    current.updateChildren(next)
                    current.updateSize(size)
                    size = 0L
                    current = next

                case _: Boolean =>
                    curName = curName.split("/").dropRight(1).mkString("/") + "/"
                    current.updateSize(size)
                    size = 0L
                    current = current.getParent

                case _ =>
            }
        }
        current.updateSize(size)
        res
    }

    class FS(val name: String) {
        private var parent: Option[FS] = None
        private var children: List[FS] = List[FS]()
        private var size = 0L

        def updateChildren(child: FS): Unit = {
            children = child :: children
        }

        def updateParent(parent: FS): Unit = {
            this.parent = Option(parent)
        }

        def updateSize(size: Long): Unit = {
            this.size += size
        }

        def getChildren: List[FS] = {
            children
        }

        def getSize: Long = {
            size
        }

        def getParent: FS = {
            parent.getOrElse(throw new IllegalStateException("Node has no parent"))
        }
    }
}