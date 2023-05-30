package binarysearch

import java.util.Scanner

object P1072 {
    val MAX = 1_000_000_000L    
    val sc = new Scanner(System.in)
    var x = 0L
    var y = 0L   

    def solve(): Unit = {
        def calaculateZ(winCount: Long, totalCount: Long): Long = {
            (winCount * 100L) / totalCount
        }

        var left = 0L
        var right = MAX
        var z = calaculateZ(y, x)
    
        def loop (): Unit = {
            if left > right then ()
            else
                val mid = (left + right) / 2L
                val z2 = calaculateZ(y+mid, x+mid)
                if z2 <= z then left = mid + 1L else right = mid - 1L
                loop()
        }

        if z >= 99 then left = -1L
        else loop()

        println(left)
    }

    def main(args: Array[String]): Unit = {

        x = sc.nextLong()
        y = sc.nextLong()

        solve()
        sc.close()
    }
}