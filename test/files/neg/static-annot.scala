

import annotation.static



class StaticInClass {
  @static val bar = 1
}


class NestedObjectInClass {
  object Nested {
    @static val blah = 2
  }
}


object NestedObjectInObject {
  object Nested {
    @static val succeed = 3
  }
}


object Conflicting {
  @static val bar = 1
}


class Conflicting {
  val bar = 45
}


object PrivateProtectedLazy {
  @static private val bar = 1
  @static private val baz = 2
  @static lazy val bam = 3
}


class PrivateProtectedLazy {
  println(PrivateProtectedLazy.bar)
  println(PrivateProtectedLazy.baz)
  println(PrivateProtectedLazy.bam)
}


class StaticDef {
  // this should not crash the compiler
  @static def x = 42
}
