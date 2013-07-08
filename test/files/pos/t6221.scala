class MyFunc[-A, +B] extends (A => B) { def apply(x: A): B = ??? }

class MyCollection[A] {
  def map[B](f: MyFunc[A, B]): MyCollection[B] = new MyCollection[B]
}

class OtherFunc[-A, +B] {}

object Test {
  implicit def functionToMyFunc[A, B](f: A => B): MyFunc[A, B] = new MyFunc

  implicit def otherFuncToMyFunc[A, B](f: OtherFunc[A, B]): MyFunc[A, B] = new MyFunc

  def main(args: Array[String]) {
    val col = new MyCollection[Int]

    // Doesn't compile: error: missing parameter type for expanded function ((x$1) => x$1.toString)
    println(col.map(_.toString))

    // Doesn't compile: error: missing parameter type
    println(col.map(x => x.toString))

    // Does compile
    println(col.map((x: Int) => x.toString))

    // Does compile (even though type params of OtherFunc not given)
    println(col.map(new OtherFunc))
  }
}