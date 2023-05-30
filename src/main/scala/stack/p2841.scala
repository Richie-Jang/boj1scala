package stack

import scala.util.chaining.*
import scala.io.StdIn
object p2841 {

    var n = 0
    var fCount = 0
    val stacks = Array.tabulate(7) {_ => List.empty[Int] }
    var count = 0

    def gets: (Int,Int) = {
        val g = StdIn.readLine.trim.split("\\s+").map(_.toInt)
        g(0) -> g(1)
    }

    def play(a: Int, b: Int): Unit = {
        var st = stacks(a)
        if st.isEmpty then 
            count += 1
            stacks(a) = b :: st
            return ()
        end if

        var lastPlay = st.head
        st = st.tail
        var isBreak = false
        while !isBreak do
            if b < lastPlay then
                count += 1
                if st.nonEmpty then 
                    lastPlay = st.head
                    st = st.tail
                else
                    isBreak = true                
                end if
            else
                isBreak = true            
        end while
        stacks(a) = st
        count += 1
        stacks(a) = b :: stacks(a)
    }

    def main(args: Array[String]): Unit = {

        gets.tap{ a =>
            n = a(0)
            fCount = a(1)
        }        

        for i <- 1 to n do
            val (a,b) = gets
            play(a,b)    
        end for
        
        println(count)
    }
}