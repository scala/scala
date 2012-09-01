// x1, x2, and x3 resulted in: symbol variable bitmap$0 does not exist in A.<init>
object A {

  try {
    lazy val x1 = 1
    println(x1)
    sys.error("!")
  } catch {
    case _: Throwable =>
      lazy val x2 = 2
      println(x2)
  } finally {
    lazy val x3 = 3
    println(x3)
  }

  if ("".isEmpty) {
    lazy val x4 = 4
    println(x4)
  }

  var b = true
  while(b) {
    lazy val x5 = 5
    println(x5)
    b = false
  }


  def method {
    try {
      lazy val x6 = 6
      println(x6)
      sys.error("!")
    } catch {
      case _: Throwable =>
        lazy val x7 = 7
        println(x7)
    } finally {
      lazy val x8 = 8
      println(x8)
    }

    if ("".isEmpty) {
      lazy val x9 = 9
      println(x9)
    }

    var b = true
    while(b) {
      lazy val x10 = 10
      println(x10)
      b = false
    }
  }
}

object Test {
  def main(args: Array[String]) {
    A.method
  }
}
