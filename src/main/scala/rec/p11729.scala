package rec

object p11729 {

    def hanoi(count: Int, start: Int, aux: Int, dest: Int, level: Int): Unit = {
        println(s"    Coming [$level] Count:$count Start:$start Aux:$aux Dest:$dest")
        if count == 1 then
            println(s"$start $dest")
        else
            hanoi(count-1, start, dest, aux, level+1)
            println(s"$start $dest")
            hanoi(count-1, aux, start ,dest, level+1)
    }

    def main(args: Array[String]): Unit = {
        hanoi(3, 1,2,3, 0)
    }
}
