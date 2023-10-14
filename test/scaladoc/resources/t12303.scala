import scala.annotation.implicitNotFound

object Test {
  private final val one   = "foo"
  private final val two   = "foo" + "bar"
  private final val three = "foo" + "bar" + "baz"
  private final val four  = "foo" + "bar" + "baz" + " oh no"

  @implicitNotFound(one)   trait One
  @implicitNotFound(two)   trait Two
  @implicitNotFound(three) trait Three
  @implicitNotFound(four)  trait Four
}

/*
 * was
newSource:6: warning: private val three in object Test is never used
  private final val three = "foo" + "bar" + "baz"
                    ^
newSource:7: warning: private val four in object Test is never used
  private final val four  = "foo" + "bar" + "baz" + " oh no"
                    ^
 */
