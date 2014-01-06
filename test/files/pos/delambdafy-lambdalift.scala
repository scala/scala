class LambdaLift {

  def enclosingMethod(capturedArg: Int): Unit = {
    def innerMethod(x: Int): Int = x + capturedArg
    val f = (y: Int) => innerMethod(y)
  }

}
