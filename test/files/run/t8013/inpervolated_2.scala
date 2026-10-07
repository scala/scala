//> using options -Werror -Xlint

import annotation._

package t8013 {

  // unsuspecting user of perverse macro
  trait User {
    import Perverse._
    val foo = "bar"
    Console.println {
      p"Hello, $foo"
    }
    Console.println {
      p(s"Hello, $foo")
    }
    Console.println {
      "Hello, $foo"
    }: @nowarn
  }
}

object Test extends App {
  new t8013.User {}
}
