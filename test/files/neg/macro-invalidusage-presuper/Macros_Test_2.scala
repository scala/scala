// scalac: -language:experimental.macros
import Impls._

class D extends { def x = macro impl } with AnyRef
