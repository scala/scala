object O {
  implicit class C(s: String) {
    def twice = s + s
  }
}

/**
// 
// We'd like to augment object O in Namers so that it also has an implicit method
object O {
  implicit class C(s: String) {
    def twice = s + s
  }
  implicit def C(s: String): C = new C(s)
}

**/
