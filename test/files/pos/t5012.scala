class D {
  object p // (program point 1)
}

class C {
  def m: D = {
    if("abc".length == 0) {
      object p       // (program point 2)
    }
    null
  }
}
