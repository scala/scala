object Test {
  def main(args: Array[String]) {
    val seq: MySeq[Undefined] = new MySeq[Floats](new Array[Float](10))
    println(10 == seq.array.length)
  }
}

sealed trait Undefined { type ArrayType <: Array[_] }
sealed trait Floats extends Undefined { type ArrayType = Array[Float] }
class MySeq[+T <: Undefined](val array: T#ArrayType)
