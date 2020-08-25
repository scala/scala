// scalac: -Xfatal-warnings -Xlint
class A { def x: Int = 3 }
class B extends A { override def x(): Int = 4 }

