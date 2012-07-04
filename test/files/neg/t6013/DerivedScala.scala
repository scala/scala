// Scala extending Scala (this case was working fine before this bug.)
class A { def foo: Int = 0 }
abstract class B extends A { def foo: Int }
class C extends B

// Scala extending Java
class DerivedScala extends Abstract
