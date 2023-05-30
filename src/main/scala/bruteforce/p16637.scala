package bruteforce

import scala.io.StdIn
import scala.math
import scala.collection.mutable

object p16637 {

  var n = 0
  var numlen = 0
  var signlen = 0
  var arr = Array.emptyLongArray
  var signArr = Array.emptyCharArray
  val signs = Set('+', '-', '*')
  val combSigns = mutable.Set.empty[Set[Int]]
  var maxValue = Long.MinValue

  def makeArr(s: String, idxNum: Int, idxSign: Int, acc: List[Char], cur: Int): Unit = {
    if cur >= s.length then
      if acc.nonEmpty then
        arr(idxNum) = acc.reverse.mkString("").toLong
      else
        val c = s(cur)
        if signs.contains(c) then
          arr(idxNum) = acc.reverse.mkString("").toLong
          signArr(idxSign) = c
          makeArr(s, idxNum + 1, idxSign + 1, List.empty, cur + 1)
        else
          makeArr(s, idxNum, idxSign, c :: acc, cur + 1)
  }

  def computeValues(s: Char, a: Long, b: Long): Long = {
    s match
      case '+' => a + b
      case '-' => a - b
      case '*' => a * b
      case _ => throw Exception("Not happened")
  }

  def dfs(cur: Int, acc: List[Int], limit: Int): Unit = {
    if acc.length > limit then ()
    else
      combSigns += acc.toSet
      for i <- cur until signlen do
        dfs(i + 2, i :: acc, limit)
  }

  def searchMaxCombination(): Unit = {
    def calculateSum(s: Set[Int]): Long = {
      val map = (
        for a <- s
          yield a -> computeValues(signArr(a), arr(a), arr(a + 1))).toMap
      // update
      var stack = if s.contains(0) then map(0) :: Nil else arr(0) :: Nil
      val starter = if s.contains(0) then 1 else 0

      def loop(cur: Int): Unit = {
        if cur >= signlen then ()
        else
          var nv = 0L
          var npos = 0
          if cur < signlen - 1 then
            val nn = cur + 1
            if s.contains(nn) then
              nv = map(nn)
              npos = (cur + 2)
            else
              nv = arr(nn)
              npos = (cur + 1)
          else
            nv = arr(cur + 1)
            npos = cur + 1
          stack = (computeValues(signArr(cur), stack.head, nv)) :: Nil
          loop(npos)
      }

      loop(starter)
      stack.head
    }

    for set <- combSigns do
      val vvv = calculateSum(set)
      maxValue = math.max(maxValue, vvv)
  }

  def main(args: Array[String]): Unit = {
    n = StdIn.readLine.trim.toInt
    signlen = n / 2
    signArr = Array.ofDim[Char](signlen)
    numlen = n - signlen
    arr = Array.ofDim[Long](numlen)
    makeArr(StdIn.readLine.trim, 0, 0, Nil, 0)
    // compute combination
    dfs(0, Nil, numlen / 2)
    searchMaxCombination()
    println(maxValue)
  }
}