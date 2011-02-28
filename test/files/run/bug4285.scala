object Test {
  def main(args: Array[String]): Unit = {
    println(scala.tools.nsc.interpreter.ILoop.run("val x = Array(1,2,3,4,5,6,7) ; val y = x transform (_ * 2) ; println(y.sum)"))
  }
}


