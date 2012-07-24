package scala.reflect

package object makro {
  @deprecated("Use scala.reflect.macros.Context instead", "2.10.0")
  type Context = scala.reflect.macros.Context
}