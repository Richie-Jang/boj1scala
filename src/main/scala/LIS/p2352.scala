package LIS

import scala.io.StdIn
import scala.collection.mutable.ArrayBuffer

object p2352 {

    var n = 0
    var lis = ArrayBuffer.empty[Int]
    var arr = Array.emptyIntArray    

    def lower_bound(target: Int): Int = {
        var st = 0
        var ed = lis.size-1
        while st < ed do
            val m = (st + ed) / 2
            val l1 = lis(m)
            if l1 < target then 
                st = m + 1
            else
                ed = m
        end while
        ed
    }

    def main(args: Array[String]): Unit = {        
        n = StdIn.readLine.trim.toInt
        arr = StdIn.readLine.trim.split("\\s+").map(_.toInt)
        // init
        lis += arr(0)
        for i <- 1 until n do
            if lis.last < arr(i) then
                lis += arr(i)
            else
                val idx = lower_bound(arr(i))
                lis(idx) = arr(i)
        end for
        println(lis.size)
    }
}