import scala.util.continuations._

object Test {

def g: List[Int] @suspendable = List(1,2,3)

def fp10: List[Int] @suspendable = {
g.map(x => x)
}

def fp11: List[Int] @suspendable = {
val z = g.map(x => x)
z
}


def fp12: List[Int] @suspendable = {
val z = List(1,2,3)
z.map(x => x)
}



def fp20: List[Int] @suspendable = {
g.map[Int,List[Int]](x => x)
}


def fp21: List[Int] @suspendable = {
val z = g.map[Int,List[Int]](x => x)
z
}

def fp22: List[Int] @suspendable = {
val z = g.map[Int,List[Int]](x => x)(List.canBuildFrom[Int])
z
}

def fp23: List[Int] @suspendable = {
val z = g.map(x => x)(List.canBuildFrom[Int])
z
}


def main(args: Array[String]) {
  reset {
    println(fp10)
    println(fp11)
    println(fp12)

    println(fp20)
    println(fp21)
    println(fp22)
    println(fp23)
  }
}

}
