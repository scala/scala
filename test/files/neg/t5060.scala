class A[+T] {
  val foo0 = {
    class AsVariantAsIWantToBe { def contains(x: T) = () }
    new AsVariantAsIWantToBe
  }
  def foo1 = {
    class VarianceIsTheSpiceOfTypes { def contains(x: T) = () }
    new VarianceIsTheSpiceOfTypes
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val xs: A[String] = new A[String]
    println(xs.foo0 contains "abc")
    println((xs: A[Any]).foo0 contains 5)
    // java.lang.NoSuchMethodException: A$AsVariantAsIWantToBe$1.contains(java.lang.String)
  }
}
