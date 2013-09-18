object Other {
  def oops(msg: String = "xxx"): Nothing = throw new Exception(msg) // should not warn
}

class NoDeads {
  def y1(arg: Any) = println("foo")
  def z1 = y1(throw new Exception)  // should warn

  def y2[T](arg: T) = println("foo")
  def z2 = y2(throw new Exception)  // should warn

  def y3[T](arg: => T) = println("foo")
  def z3 = y3(throw new Exception)  // should not warn: by name arg

  def nowarn1 = synchronized { throw new Exception } // should not warn: synchronized should be by name

  def nowarn2: Int = synchronized { // should not warn
      val i = 10 + 2
      return i
  }
  def nowarn3: Int = synchronized { // should not warn
      val i = 10 + 2
      i
  }

  def nowarn4: String = Other.oops("don't warn about me") // should not warn

  def yeswarn1 = synchronized {
    throw new Exception // should warn
    5 * 5
  }
  def yeswarn2: Int = synchronized {
    throw new Exception // should warn
    return 5
  }
}

