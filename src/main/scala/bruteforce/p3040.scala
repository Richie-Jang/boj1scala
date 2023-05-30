package bruteforce

import scala.io.StdIn
import scala.util.control.Breaks

object p3040 {

  var n = 9
  var totalSum = 0
  var arr = Array.emptyIntArray

  def solve(): Unit = {
    var founded = false
    def selectTwo(cur: Int, idxSet: Set[Int]): Unit = {
      if idxSet.size == 2 then
        var curSum = totalSum
        idxSet.foreach(i => curSum -= arr(i))
        if curSum == 100 then
          founded = true
          for i <- arr.indices if !idxSet.contains(i) do
            println(s"${arr(i)}")
        end if
      else
        for idx <- cur until arr.length do
          if !founded then selectTwo(idx+1, idxSet + idx)
      end if
    }
    selectTwo(0, Set.empty)
  }

  def main(args: Array[String]): Unit = {
    arr = Array.tabulate(n) { _ =>
      val i = StdIn.readLine.trim.toInt
      totalSum += i
      i
    }
    solve()
  }

}
