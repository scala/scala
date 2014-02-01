
package object t7322 {
  implicit class X(sc: StringContext) {
    def x_?(args: Any*) = "hi there"
  }
}
package t7322 {
  trait Y {
    x_?"junk"  // assume that if it compiles, it works
  }
}
