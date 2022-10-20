// scalac: -Xlint -Werror

@deprecated("we won't use this package any more", since="0.9-RC1")
package object p {
  def then = 42           // future reserved, but suppressed by available package object
}
@deprecated("also defunct", since="0.9-RC1")
package q {
  class C
}
