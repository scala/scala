object Test extends Application {

  println(test1)
  println(test2)
  println(test3)
  println(test4)
  println(test5)
  try { println(test6) } catch { case _ => println("OK") }
  println(test7)
  try { println(test8) } catch { case _ => println("OK") }
  println(test9)
  println(test10)
  println(test11)
  println(test12)

  def test1 = {
    var x = 1
    try {
      x = 2
    } catch {
      case _: NullPointerException => x = 3
      case _ => x = 4
    }
    x
  }

  def test2 = {
    var x = 1
    try {
      x = 2
      try {
        x = 21
      } catch {
        case _ => x = 22
      }
      x = 23
    } catch {
      case _: NullPointerException => x = 3
      case _ => x = 4
    }
    x
  }

  def test3 = {
    var x = 1
    try {
      try{x = 2} catch { case _ => x = 4 }
    } catch {
      case _: NullPointerException => x = 3
      case _ => x = 4
    }
    x
  }

  def test4 = {
    var x = 1
    try {
      x = 2
    } catch {
      case _: NullPointerException => x = 3
      case _ => x = 4
    }
    try {
      x = 5
    } catch {
      case _: NullPointerException => x = 6
    }
    x
  }

  def test5 = {
    var x = 1
    try {
      x = 2
    } catch {
      case _: NullPointerException => try { x = 3 } catch { case f => throw f }
      case _ => x = 4; try { x = 41 } catch { case _: Exception => x = 42 }; x = 43
    }
    x
  }

  def test6: Int = {
    var x = 1
    try {
      x = 2
      (null: String).toString
    } catch {
      case e: NullPointerException =>
        throw e
      case _ =>
        x = 3
        return 1000
    } finally {
      x = 4
      println(x)
    }
    x
  }

  def test7 = {
    var x = 1
    try {
      x = 2
    } finally {
      try {
        x = 4
      } catch {
        case _ => x = 5
      }
    }
    x
  }

  def test8 = {
    var x = 1
    try {
      throw new NullPointerException
    } catch {
      case e => throw e
    }
    x
  }

  def test9 = {
    try { "" match {
      case s: String => 10
    }} catch { case _ => 20 }
  }

  var x10 = 1
  def test10: Int = {
    try { 1 }
    catch { case e if (x10 == 1) => 1 }
  }

   def test11 {
    try { () }
    catch { case e => () }
  }

  class E1 extends Exception
  class E2 extends Exception
  class E3 extends Exception

  def test12_impl(op: => Int) = try {
    op
  } catch {
    case e: E1 => 2
    case e: E2 => 3
    case e: E3 => 4
  }
  def test12 =
    test12_impl(1) +
    test12_impl(throw new E1) +
    test12_impl(throw new E2) +
    test12_impl(throw new E3)
}
