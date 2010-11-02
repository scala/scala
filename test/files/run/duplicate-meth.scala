
trait Base {
  private val secure_# = 10l
}

class TestUser extends Base {
  def clsMeth(x: Int) = x
  private def foo(x: Int) = x
}

object TestUser extends TestUser {
  def objMeth = "a"

  private def foo(x: Int) = x
}

object Test {
  def main(args: Array[String]) {
    TestUser.objMeth
    // no-op, just check that it passes verification
    println("verified!")
  }
}
