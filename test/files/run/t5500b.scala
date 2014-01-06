import scala.{specialized => spec}

class C1A[
  @spec(Double, Int, AnyRef) A,
  @spec(Double, Int, AnyRef) B
]

class C1B[
  @spec(Double, Int, AnyRef) A,
  @spec(Double, Int, AnyRef) B
](v: A)

class C1C[
  @spec(Double, Int, AnyRef) A,
  @spec(Double, Int, AnyRef) B
](v:A, w:B)

object Test {
  def main(args:Array[String]) {
    println(new C1A[String, Int].getClass.getName)
    println(new C1A[String, Double].getClass.getName)
    println(new C1A[String, String].getClass.getName)
    println(new C1A[Int, Int].getClass.getName)
    println(new C1A[Int, Double].getClass.getName)
    println(new C1A[Int, String].getClass.getName)
    println(new C1A[Double, Int].getClass.getName)
    println(new C1A[Double, Double].getClass.getName)
    println(new C1A[Double, String].getClass.getName)

    println(new C1B[String, Int]("abc").getClass.getName)
    println(new C1B[String, Double]("abc").getClass.getName)
    println(new C1B[String, String]("abc").getClass.getName)
    println(new C1B[Int, Int](1).getClass.getName)
    println(new C1B[Int, Double](1).getClass.getName)
    println(new C1B[Int, String](1).getClass.getName)
    println(new C1B[Double, Int](1d).getClass.getName)
    println(new C1B[Double, Double](1d).getClass.getName)
    println(new C1B[Double, String](1d).getClass.getName)

    println(new C1C("abc", 123).getClass.getName)
    println(new C1C("abc", 123).getClass.getName)
    println(new C1C("a", 1d).getClass.getName)
    println(new C1C("a", "a").getClass.getName)
    println(new C1C(1, 1).getClass.getName)
    println(new C1C(1, 1d).getClass.getName)
    println(new C1C(1, "a").getClass.getName)
    println(new C1C(1d, 1).getClass.getName)
    println(new C1C(1d, 1d).getClass.getName)
    println(new C1C(1d, "a").getClass.getName)
  }
}
