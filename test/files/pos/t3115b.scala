// scalac: -Xlint -Werror

package p {
  @deprecated("we won't use this package any more", since="0.9-RC1")
  object `package` {
    def then = 42           // future reserved, but suppressed by available package object
  }
}
