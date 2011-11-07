package other

object IllegalAccess {
  def main(args: Array[String]) {
    val x = (new test.ScalaBipp).make.get.asInstanceOf[test.ScalaBipp].f() 
    println(x)
    val y = (new test.ScalaBipp).make.get.f()
    println(y)
    val u = (new test.ScalaBipp).make.get.asInstanceOf[test.ScalaBipp].t
    println(u)
    val v = (new test.ScalaBipp).make.get.t
    println(v)
    val sb: test.ScalaBipp = (new test.ScalaBipp).make.get
    val z = sb.t
    println(z)
  }
}
