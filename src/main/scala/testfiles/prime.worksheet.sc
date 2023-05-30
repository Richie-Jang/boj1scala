import scala.collection.mutable.HashMap
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ListBuffer
// Prime algorithm test
val graph = HashMap.empty[Int, ListBuffer[(Int,Int)]]

val V = 5
// input
graph.addOne(0 -> ListBuffer(1->2, 4->4, 3->8))
graph.addOne(1 -> ListBuffer(0->2, 2->3))
graph.addOne(2 -> ListBuffer(1->3, 4->1, 3->5))
graph.addOne(3 -> ListBuffer(2->5, 4->7, 0->8))
graph.addOne(4 -> ListBuffer(0->4, 2->1, 3->7))

given cmpPQ: Ordering[(Int,Int)] with
    def compare(a: (Int,Int), b: (Int,Int)): Int = {
        a._2.compareTo(b._2) * -1
    }      

val parents = Array.tabulate(V)(a => a)

def computeMST(): Unit = {
    val dist = Array.ofDim[Int](V)
    val visits = Array.ofDim[Boolean](V)    
    for v <- graph do
        dist(v._1) = Int.MaxValue        
    // select last one
    val selectItem = graph.last._1
    dist(selectItem) = 0
    parents(selectItem) = -1
    val pq = PriorityQueue.empty[(Int,Int)](cmpPQ)
    pq.enqueue(selectItem -> dist(selectItem))

    while pq.nonEmpty do
        val (cur, cost) = pq.dequeue
        if cost <= dist(cur) then
            println(s"$cur => $cost")
            visits(cur) = true
            for nv <- graph(cur) if !visits(nv._1) do
                if nv._2 < dist(nv._1) then
                    dist(nv._1) = nv._2
                    parents(nv._1) = cur
                    println(s"Update parents : ${nv._1} <= $cur")
                    pq.enqueue(nv._1 -> dist(nv._1))
}

computeMST()

parents.mkString(",")

for v <- parents.indices if parents(v) != -1 do
    println(s"$v => ${parents(v)}")
    
