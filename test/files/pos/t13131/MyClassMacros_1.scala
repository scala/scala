package macros

import scala.language.experimental.macros

trait MyClassMacros {

  implicit def derived[T]: MyClass[T] = macro MyClassMacroImpl.derived[T]

}
