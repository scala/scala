package scala.reflect.internal

package object annotations {
  @deprecated("Use scala.annotation.compileTimeOnly instead", "2.11.0")
  type compileTimeOnly = scala.annotation.compileTimeOnly
}