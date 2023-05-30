package math.p4134

import java.io.BufferedReader
import scala.collection.mutable
import scala.math
import java.io.InputStreamReader

object Main {    
    var br : BufferedReader = _
    var n = 0    
    val sb = mutable.StringBuilder()
    var arr = Array.emptyBooleanArray

    def preparing(): Unit = {
        val max = 4_000_000_001L        
        val mm = math.sqrt(max).toInt+10
        arr = Array.tabulate(mm)(_ => true)
        arr(0) = false
        arr(1) = false
        for i <- 2 until mm do
            var j = 2        
            while j * i < mm do
                arr(i * j) = false
                j += 1
    }

    def isPrime(l: Long): Boolean = {
        if l == 0L || l == 1L then false
        else if l == 2L then true
        else if l % 2L == 0 then false
        else
            val mm = math.sqrt(l).toInt
            for i <- 2 to mm if arr(i) do
                if l % i.toLong == 0L then return false
            true
    }

    def solve(a: Long): Unit = {
        def loop(v: Long): Unit = {            
            if isPrime(v) then sb.append(v).append(System.lineSeparator)
            else loop(v+1L)
        }

        loop(a)
    }

    def main (args: Array[String]): Unit = {
        br = new BufferedReader(new InputStreamReader(System.in))
        n = br.readLine.trim.toInt   
        preparing()     
        for i <- 1 to n do
            solve(br.readLine.trim.toLong)
        print(sb.toString)                
        br.close()
    }
}

