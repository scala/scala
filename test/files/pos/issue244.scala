trait T { lazy val overloaded: String = "a" }
class C extends T { def overloaded(a: String): String = "b" }
