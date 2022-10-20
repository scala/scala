// scalac: -Xlint -Werror

package p {
  @deprecated("we won't use this package any more", since="0.9-RC1")
  object `package`

  // TODO should suppress, but is not in range; should look up elements by position for semantics
  @annotation.nowarn
  class C {
    def then = 42           // future reserved, but suppressed by available package object
  }
}
