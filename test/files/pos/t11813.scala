//> using options -Werror -Xlint:implicit-recursion
//
package warner

trait Target

trait Provider {
  implicit val provided: Target = new Target {}
}

object stuff extends Provider

object ok {
  implicit val provided: Target = new Target {}
}

object Main extends App {
  locally {
    import stuff._
    implicitly[Target]
  }
  locally {
    import ok._
    implicitly[Target]
  }
}

/* Was:
impl.scala:16: warning: Implicit resolves to enclosing value provided
  println(implicitly[Target])
                    ^
one warning found
 */
