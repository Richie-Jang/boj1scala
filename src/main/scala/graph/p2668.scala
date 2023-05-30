package graph

import java.io.{BufferedReader, InputStreamReader}
import scala.collection.mutable

object p2668 {

    var n: Int = 0
    var grid: Array[Int] = Array.empty

    def solve(): Unit = {
        val res = mutable.TreeSet.empty[Int]
        val ckNodes = Array.ofDim[Boolean](n+1)

        def cycleCheck(curNode: Int, endNode: Int): Unit = {
            var node = curNode

            for (i <- 1 to n) {
                if (node == endNode) {
                    res.add(node)
                    ckNodes(node) = true
                    return
                }

                node = grid(node)
            }
        }

        for (i <- 1 to n) {
            cycleCheck(grid(i), i)
        }

        val sb = new mutable.StringBuilder()
        sb.append(res.size)
        sb.append(System.lineSeparator())
        for (v <- res) {
            sb.append(v)
            sb.append(System.lineSeparator())
        }

        print(sb.toString())
    }

    def main(args: Array[String]): Unit = {
        val br = new BufferedReader(new InputStreamReader(System.in))
        n = br.readLine().toInt
        grid = Array.ofDim[Int](n+1)
        for (i <- 1 to n) {
            grid(i) = br.readLine().toInt
        }
        solve()
        br.close()
    }
}
