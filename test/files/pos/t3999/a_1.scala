package foo

class Outside

package object bar {
  class Val(b: Boolean)
  implicit def boolean2Val(b: Boolean) = new Val(b)
  implicit def boolean2Outside(b: Boolean) = new Outside
}