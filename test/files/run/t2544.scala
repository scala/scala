object Test {
  object Foo extends Seq[Int] {
    def apply(i: Int) = i
    def length = 4
    def iterator = Iterator(0,1,2,3,4)
  }
  def main(args: Array[String]) = {
    println(Foo indexWhere(_ >= 2,1))
    println(Foo.toList indexWhere(_ >= 2,1))
    println(Foo segmentLength(_ <= 3,1))
    println(Foo.toList segmentLength(_ <= 3,1))
    println(Foo lengthCompare 7)
    println(Foo.toList lengthCompare 7)
    println(Foo lengthCompare 2)
    println(Foo.toList lengthCompare 2)
    println(Foo lengthCompare 5)
    println(Foo.toList lengthCompare 5)
  }
}