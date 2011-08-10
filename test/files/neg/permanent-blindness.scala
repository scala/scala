package foo {
  class Bippy
  object Bop {
    def fff = 5
  }
  object Dingus
}

package bar {
  import foo.{ Bippy, Bop, Dingus }

  class Bippy
  object Bop
  object Dingus


  class Ding {
    def fff = 5

    def g = new Bippy
  }
}