object Test {
  def main(args: Array[String]) {
    try {
      println("...")
    }
    finally {
      try {
        println("...")
      }
      finally {
        try {
          println("...")
        }
        catch {
          case ct: scala.util.control.ControlThrowable => throw(ct)
          case t: Throwable => t.printStackTrace()
        }
      }
    }
  }
}
