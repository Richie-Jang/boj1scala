package stack

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object p10799 {

    case class Stick(stidx: Int, edidx: Int)

    val stack: mutable.Stack[Int] = mutable.Stack.empty[Int]
    val lasers: ArrayBuffer[Int] = mutable.ArrayBuffer.empty[Int]
    val sticks: ArrayBuffer[Stick] = mutable.ArrayBuffer.empty[Stick]
    def main(args: Array[String]): Unit = {
        val l = io.StdIn.readLine()
        for i <- l.indices do {
            val c = l(i)
            c match {
                case '(' => stack.push(i)
                case _ =>
                    val lastPos = stack.pop()
                    val count = i - lastPos
                    if count == 1 then lasers += lastPos
                    else sticks += Stick(lastPos, i)
            }
        }

        // counting
        var count = 0
        for stick <- sticks do {
            val (l, r) = stick.stidx -> stick.edidx
            var inLaserCount = 0
            for laser <- lasers do {
                if laser > l && laser < r then {
                    inLaserCount += 1
                }
            }
            count += (inLaserCount + 1)
        }

        println(count)
    }
}
