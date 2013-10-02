


import collection.mutable.{Stack, StackProxy}



class A extends StackProxy[Int] {
  val self = Stack[Int]()
}


object Test {

  def main(args: Array[String]) {
    val a = new A

    a push 3
    a push 4
    a push 5

    a.push(6, 7, 8)

    println(a)

    a.pop
  }

}
