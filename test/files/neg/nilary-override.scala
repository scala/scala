// scalac: -Werror -Xlint:nullary-override
//
class A { def x: Int = 3 }
class B extends A { override def x(): Int = 4 }

class C { def x(): Int = 3 }
class D extends C { override def x: Int = 4 }

class J { override def toString = "happy J" }
