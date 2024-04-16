//> using options -Werror
//
package foo {

class Bippy

class BIPPY

object Dingo
object DINGO

case class Hyrax()
object HyRaX

class Wacko
object wackO

package object bar
package bar {
  class Package
}

object O extends App {
  object a_! {
    def foo = 1
  }

  object a_ {
    object bang {
      def foo = 2
    }
  }
  println(a_!.foo)
  println(a_.bang.foo)
}
}
