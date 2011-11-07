// ticket #421
object Test extends App {

  def transpose[A: ClassManifest](xss: Array[Array[A]]) = {
    for (i <- Array.range(0, xss(0).length)) yield
      for (xs <- xss) yield xs(i)
  }

  def scalprod(xs: Array[Double], ys: Array[Double]) = {
    var acc = 0.0 
    for ((x, y) <- xs zip ys) acc = acc + x * y  
    acc
  }

  def matmul(xss: Array[Array[Double]], yss: Array[Array[Double]]) = {
    val ysst = transpose(yss) 
    val ysst1: Array[Array[Double]] = yss.transpose
    assert(ysst.deep == ysst1.deep)
    for (xs <- xss) yield
      for (yst <- ysst) yield 
        scalprod(xs, yst)
  }

  val a1 = Array(Array(0, 2, 4), Array(1, 3, 5))
  println(transpose(a1).deepMkString("[", ",", "]"))

  println(matmul(Array(Array(2, 3)), Array(Array(5), Array(7))).deepMkString("[", ",", "]"))
  
  println(matmul(Array(Array(4)), Array(Array(6, 8))).deepMkString("[", ",", "]"))
}
