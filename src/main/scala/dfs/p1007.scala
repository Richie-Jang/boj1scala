package dfs

import java.util.Scanner
import scala.math
object P1007:

    case class Pt(x: Int, y: Int)

    var t = 0
    var n = 0        

    def solve(sc: Scanner): Unit =
        n = sc.nextInt()
        val inps = Array.ofDim[Pt](n)
        for i <- 0 until n do
            inps(i) = Pt(sc.nextInt(), sc.nextInt())

        var minValue = Long.MaxValue
        val selects = Array.ofDim[Boolean](n)

        def compute(): Unit =
            var px = 0L
            var py = 0L
            for i <- selects.indices do
                if selects(i) then 
                    px = px + inps(i).x.toLong
                    py = py + inps(i).y.toLong
                else 
                    px = px - inps(i).x.toLong
                    py = py - inps(i).y.toLong
            val dist = px * px + py * py
            minValue = math.min(dist, minValue)

        def dfs(cur: Int, depth: Int): Unit =
            if depth == n / 2 then
                compute()
            else
                for i <- cur until n do
                    if !selects(i) then
                        selects(i) = true
                        dfs(i, depth+1)
                        selects(i) = false
        
        dfs(0, 0)
        println(math.sqrt(minValue))

    def main(args: Array[String]): Unit =
        val sc = Scanner(System.in)
        t = sc.nextInt()
        for i <- 0 until t do
            solve(sc)
        sc.close()

end P1007