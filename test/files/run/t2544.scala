object Test {
  object Foo extends Seq[Int] {
    def apply(i: Int) = i
    def length = 5
    def iterator = Iterator(0,1,2,3,4)
  }
  def lengthEquiv(result: Int) = println(
    if (result < 0) -1
    else if (result == 0) 0
    else 1
  )

  def main(args: Array[String]) = {
    println(Foo indexWhere(_ >= 2,1))
    println(Foo.toList indexWhere(_ >= 2,1))
    println(Foo segmentLength(_ <= 3,1))
    println(Foo.toList segmentLength(_ <= 3,1))
    lengthEquiv(Foo lengthCompare 7)
    lengthEquiv(Foo.toList lengthCompare 7)
    lengthEquiv(Foo lengthCompare 2)
    lengthEquiv(Foo.toList lengthCompare 2)
    lengthEquiv(Foo lengthCompare 5)
    lengthEquiv(Foo.toList lengthCompare 5)
  }
}
