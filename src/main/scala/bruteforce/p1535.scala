package bruteforce

import scala.io.StdIn
import scala.util.chaining.*

object p1535 {

  var n = 0
  var healths = Array.empty[Int]
  var happies = Array.empty[Int]

  val gets: () => Array[Int] = { () =>
    StdIn.readLine().split("\\s+").map(_.toInt)
  }

  def solve(): Unit = {
    var set = Set.empty[Int]
    def loop(cur: Int, health : Int, hap: Int, selectedHap: Int): Unit = {
      if cur >= n then
        if health > 0 then
          set = set + hap
        else
          set = set + (hap - selectedHap)
        return ()
      if health <= 0 then
        set = set + (hap - selectedHap)
        return ()
      for j <- cur+1 to n do
        loop(j, health-healths(cur), hap+happies(cur), happies(cur))
    }
    for i <- 0 until n do
      loop(i, 100, 0, 0)
    println(set.max)
  }

  def main(args: Array[String]): Unit = {
    n = StdIn.readLine.trim.toInt
    healths = gets()
    happies = gets()
    solve()
  }
}
