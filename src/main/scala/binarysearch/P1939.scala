package binarysearch

import java.io.BufferedInputStream
import java.util.Scanner
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object P1939 {

  case class Node(pos: Int, weight: Int)

  val sc = new Scanner(new BufferedInputStream(System.in))
  var n = 0
  var m = 0
  var maxWeight = Int.MinValue
  var adjs = Array.empty[ListBuffer[Node]]
  var startPos = 0
  var endPos = 0
  var vs = Array.emptyBooleanArray

  def dfs (pos: Int, w: Int): Boolean = {
    vs(pos) = true
    if pos == endPos then return true
    for nn <- adjs(pos) do
      if !vs(nn.pos) && w <= nn.weight then
        val rrr = dfs(nn.pos, w)
        if rrr then return true
    false
  }

  def solve(): Unit = {

    var left = 0
    var right = maxWeight

    while left <= right do
      val mid = (left + right) / 2
      if dfs(startPos, mid) then left = mid+1 else right = mid-1
      vs.indices.foreach(a => vs(a) = false)
    end while

    println(left-1)

  }

  def main(args: Array[String]): Unit = {
    n = sc.nextInt()
    m = sc.nextInt()
    vs = Array.ofDim(n+1)
    adjs = Array.tabulate(n+1)(_ => ListBuffer.empty[Node])
    for i <- 0 until m do
      val br1 = sc.nextInt()
      val br2 = sc.nextInt()
      val weight = sc.nextInt()
      if maxWeight < weight then maxWeight = weight
      adjs(br1) += Node(br2, weight)
      adjs(br2) += Node(br1, weight)
    // for
    startPos = sc.nextInt()
    endPos = sc.nextInt()

    solve()
    sc.close()
  }

}

