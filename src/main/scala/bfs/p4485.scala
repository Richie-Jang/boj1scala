package bfs

import scala.collection.immutable.TreeSet

object p4485 {

    case class Pos(x: Int, y: Int)

    val next: List[(Int, Int)] = List(
        -1 -> 0,
        1 -> 0,
        0 -> 1,
        0 -> -1
    )

    def nextPoss(x: Int, y: Int, limit: Int): List[(Int,Int)] = {
        next.map { ab => (ab._1 + x, ab._2 + y) }
            .filter {ab => ab._1 >= 0 && ab._1 < limit && ab._2 >= 0 && ab._2 < limit }
    }

    def solve(n: Int, count: Int): Unit = {
        val grid = Array.ofDim[Int](n, n)
        for (i <- 0 until n) {
            val s = io.StdIn.readLine().split("\\s+")
            for (j <- s.indices) {
                grid(i)(j) = s(j).toInt
            }
        }

        val q = scala.collection.mutable.Queue[Pos]()
        val vs = Array.fill[Long](n,n)(Int.MaxValue)
        q.enqueue(Pos(0,0))
        vs(0)(0) = grid(0)(0)
        while (q.nonEmpty) {
            val p = q.dequeue()
            val ns = nextPoss(p.x, p.y, n)
            for (p1 <- ns) {
                if (vs(p1._2)(p1._1) <= vs(p.y)(p.x) + grid(p1._2)(p1._1)) {

                } else {
                    vs(p1._2)(p1._1) = vs(p.y)(p.x) + grid(p1._2)(p1._1)
                    q.enqueue(Pos(p1._1, p1._2))
                }
            }
        }

        println(s"Problem $count: ${vs(n-1)(n-1)}")

    }

    def main(args: Array[String]): Unit = {
        var count = 1
        var isGo = true
        while (isGo) {
            val n = io.StdIn.readLine().toInt
            if (n == 0) {
                isGo = false
                return
            } else {
                solve(n, count)
            }
            count += 1
        }
    }

}
