import annotation._
import elidable._

trait T {
  @elidable(FINEST) def f1()
  @elidable(SEVERE) def f2()
  @elidable(FINEST) def f3() = assert(false, "Should have been elided.")
  def f4()
}

class C extends T {
  def f1() = println("Good for me, I was not elided. C.f1")
  def f2() = println("Good for me, I was not elided. C.f2")
  @elidable(FINEST) def f4() = assert(false, "Should have been elided.")
}

object O {
  @elidable(FINEST) def f1() = assert(false, "Should have been elided.")
  @elidable(INFO) def f2() = assert(false, "Should have been elided.")
  @elidable(SEVERE) def f3() = println("Good for me, I was not elided. O.f3")
  @elidable(INFO) def f4 = assert(false, "Should have been elided (no parens).")
}

object Test {
  @elidable(FINEST) def f1() = assert(false, "Should have been elided.")
  @elidable(INFO) def f2() = assert(false, "Should have been elided.")
  @elidable(SEVERE) def f3() = println("Good for me, I was not elided. Test.f3")
  @elidable(INFO) def f4 = assert(false, "Should have been elided (no parens).")

  @elidable(FINEST) def f5() = {}
  @elidable(FINEST) def f6() = true
  @elidable(FINEST) def f7() = 1:Byte
  @elidable(FINEST) def f8() = 1:Short
  @elidable(FINEST) def f9() = 1:Char
  @elidable(FINEST) def fa() = 1
  @elidable(FINEST) def fb() = 1l
  @elidable(FINEST) def fc() = 1.0f
  @elidable(FINEST) def fd() = 1.0
  @elidable(FINEST) def fe() = "s"

  def main(args: Array[String]): Unit = {
    f1()
    f2()
    f3()
    f4
    O.f1()
    O.f2()
    O.f3()
    O.f4

    val c = new C
    c.f1()
    c.f2()
    c.f3()
    c.f4()

    // make sure a return value is still available when eliding a call
    println(f5())
    println(f6())
    println(f7())
    println(f8())
    println(f9().toInt)
    println(fa())
    println(fb())
    println(fc())
    println(fd())
    println(fe())

    // this one won't show up in the output because a call to f1 is elidable when accessed through T
    (c:T).f1()

    // Test whether the method definitions are still available.
    List("Test", "Test$", "O", "O$", "C", "T") foreach { className =>
      List("f1", "f2", "f3", "f4") foreach { methodName =>
        Class.forName(className).getMethod(methodName)
      }
    }
    List("Test", "Test$") foreach { className =>
      List("f5", "f6", "f7", "f8", "f9", "fa", "fb", "fc", "fd", "fe") foreach { methodName =>
        Class.forName(className).getMethod(methodName)
      }
    }
  }
}
