//> using options -Yimports:scala,hello.world.minidef

// import at top level or top of package disables implicit import.
// the import can appear at any statement position, here, end of package.
// It masks the whole unit, even if a usage is typed before the import is completed.

import hello.world.minidef.{Magic => Answer}

package p {
  class C {
    val v: Numb = Answer
    def greet() = println("hello, world!")
  }
}
