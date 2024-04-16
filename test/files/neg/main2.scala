//> using options -Xfatal-warnings -Xmain-class p.X
//

// emit a warning

package p {
  object X {
    def main(args: Array[String]): Unit = ()
  }
  trait X
}

// no warn because not the designated main

package q {
  object Main {
    def main(args: Array[Int]) = ()
  }
}
