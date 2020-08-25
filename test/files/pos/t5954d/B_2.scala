// scalac: -Xfatal-warnings -Xdev
package p {
  trait T {
    class B
    object B
  }
  package object base extends T
}
