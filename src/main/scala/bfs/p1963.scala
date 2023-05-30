package bfs

import java.io.{BufferedReader, InputStreamReader}
import scala.collection.mutable
import scala.math

object p1963 {

    var n: Int = 0

    def createPrimeNumberSet(): Set[Int] = {
        var res = Set.empty[Int]
        res = res + 2
        res = res + 3
        for v <- 4 to 9999 do
            val half = v / 2
            val noneValue = (2 to half).find(v % _ == 0)
            if noneValue.isEmpty then res = res + v
        res
    }

    def solve(a: Int, b: Int, primes: Set[Int]): String = {
        def intArr(v: Int): Array[Int] = {
            val res = Array.ofDim[Int](4)
            res(0) = v / 1000
            res(1) = (v / 100) % 10
            res(2) = (v / 10) % 10
            res(3) = v % 10
            res
        }

        def arrToInt(arr: Array[Int]): Int = {
            var res = 0
            res += arr(0) * 1000
            res += arr(1) * 100
            res += arr(2) * 10
            res += arr(3)
            res
        }

        import scala.util.control.Breaks.*

        val q = mutable.Queue.empty[Int]
        var map = Map.empty[Int, Int]
        q.enqueue(a)
        map = map + (a -> 0)
        while q.nonEmpty do
            val p = q.dequeue()
            val count = map(p)
            if b == p then return count.toString
            val ns = intArr(p)
            for i <- 0 until 4 do
                for j <- 0 to 9 do
                    breakable {
                        if i == 0 && j == 0 then break()
                        val orgv = ns(i)
                        ns(i) = j
                        val vi = arrToInt(ns)
                        ns(i) = orgv
                        if !primes.contains(vi) then break()
                        if !map.contains(vi) then
                            map = map + (vi -> (count+1))
                            q.enqueue(vi)
                    }
                end for
            end for
        end while

        "Impossible"
    }

    def main(args: Array[String]): Unit = {
        val br = new BufferedReader(new InputStreamReader(System.in))
        n = br.readLine().toInt
        val rr = createPrimeNumberSet()
        val sb = mutable.StringBuilder()
        for _ <- 1 to n do
            val ab = br.readLine().split("\\s+")
            sb.append(solve(ab(0).toInt, ab(1).toInt, rr))
            sb.append(System.lineSeparator())
        br.close()
        println(sb.toString())

    }
}
