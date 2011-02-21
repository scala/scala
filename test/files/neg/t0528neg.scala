trait Sequ[+A] {
  def toArray: Array[T forSome {type T <: A}]
}

class RichStr extends Sequ[Char] {
  // override to a primitive array
  def toArray: Array[Char] = new Array[Char](10)
}

object Foo extends App {
  val x: RichStr = new RichStr

  println(x.toArray) // call directly
  println((x: Sequ[Char]).toArray) // calling through the bridge misses unboxing
}
