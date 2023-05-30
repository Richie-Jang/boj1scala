package tree

import scala.io.StdIn

object p3584 {

  class Tree(var node: Int, var parent: Tree, var childs: List[Tree])

  def gets: Array[Int] = {
    StdIn.readLine.trim.split("\\s+").map(_.toInt)
  }

  def solve(): Unit = {
    val m = StdIn.readLine.trim.toInt
    val trees = Array.tabulate(m+1)(s => Tree(s, null, List.empty)).drop(1)
    for i <- 1 until m do
      val g = gets
      trees(g(0)).childs = trees(g(1)) :: trees(g(0)).childs
      trees(g(1)).parent = trees(g(0))

    val (a1, a2) =
      val g = gets
      g(0) -> g(1)

  }

  def main(args: Array[String]): Unit = {

    val t = StdIn.readLine.trim.toInt
    for i <- 1 to t do
      solve()
    end for

  }

}
