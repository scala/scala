import annotation.unused

trait Base {
  @unused private val secure_# = 10L
}

class TestUser extends Base {
  def clsMeth(x: Int) = x
  @unused private def foo(x: Int) = x
}

object TestUser extends TestUser {
  def objMeth = "a"

  @unused private def foo(x: Int) = x
}

object Test {
  def main(args: Array[String]): Unit = {
    TestUser.objMeth
    // no-op, just check that it passes verification
    println("verified!")
  }
}
