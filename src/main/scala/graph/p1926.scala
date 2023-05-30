package graph

import scala.annotation.tailrec

object p1926 {

    var grid: Array[Array[Int]] = Array.empty
    var vs: Array[Array[Boolean]] = Array.empty

    var count: Int = 0
    var maxSize: Int = 0

    val moves = List(
        -1 -> 0,
        1 -> 0,
        0 -> -1,
        0 -> 1
    )

    def getNexts(x: Int, y: Int): List[(Int,Int)] = {
        moves.map {s => (s._1 + x) -> (s._2 + y) }.filter { s => s._1 >= 0 && s._1 < grid(0).length
            && s._2 >= 0 && s._2 < grid.length && !vs(s._2)(s._1) && grid(s._2)(s._1) == 1 }
    }

    def dfs(startx: Int, starty: Int, acc: Int): Int = {
        vs(starty)(startx) = true
        val ms = getNexts(startx, starty)
        if (ms.isEmpty) {
            return acc
        }
        var newAcc = acc
        for (m <- ms) {
            if (!vs(m._2)(m._1)) {
                newAcc = dfs(m._1, m._2, newAcc+1)
            }
        }
        newAcc
    }

    def main(args: Array[String]): Unit = {
        val (y,x) = {
            val s = io.StdIn.readLine().split("\\s+")
            s(0).toInt -> s(1).toInt
        }
        grid = Array.ofDim[Int](y,x)
        vs = Array.ofDim[Boolean](y,x)

        for (yi <- grid.indices) {
            val s = io.StdIn.readLine().split("\\s+")
            for (xi <- s.indices) {
                grid(yi)(xi) = s(xi).toInt
            }
        }


        for (yi <- grid.indices) {
            for (xi <- grid(0).indices) {
                if (!vs(yi)(xi) && grid(yi)(xi) != 0) {
                    count += 1
                    val g = dfs(xi, yi, 1)
                    //println(s"Found Count: $g")
                    if (g > maxSize) {
                        maxSize = g
                    }
                }
            }
        }

        println(count)
        println(maxSize)
    }
}
