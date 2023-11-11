package AoC2022
package Helpers

import scala.io.Source

object Readers {
    def readFile(path: String): List[String] = {
        val file = Source.fromFile(path)
        val data = file.getLines().map(it => it.replace("\r", "")).map(it => it.replace("\n", "")).toList
        file.close()
        data
    }
}
