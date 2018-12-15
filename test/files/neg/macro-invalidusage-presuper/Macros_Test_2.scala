import scala.language.experimental.macros

class D extends { def x = macro impl } with AnyRef