//> using options -Werror -Xsource:3

class C1(x: String) {
  def x: Int = 4 // warn in Xsource:3
}

class C2(private[this] val x: String) {
  def x: Int = 4 // warn in Xsource:3
}

class C3(private val x: String) {
  def x: Int = 4 // err
}

object o4 {
  class C4(private[o4] val x: String) {
    def x: Int = 4 // err
  }
}

class C5(protected val x: String) {
  def x: Int = 4 // err
}

object o6 {
  class C6(protected[o6] val x: String) {
    def x: Int = 4 // err
  }
}

class C7(val x: String) {
  def x: Int = 4 // err
}

class D1(x: String) {
  def x(): Int = 4 // warn in Xsource:3
}

class D2(private[this] val x: String) {
  def x(): Int = 4 // warn in Xsource:3
}

class D3(private val x: String) {
  def x(): Int = 4 // err
}

object p4 {
  class D4(private[p4] val x: String) {
    def x(): Int = 4 // err
  }
}

class D5(protected val x: String) {
  def x(): Int = 4 // err
}

object p6 {
  class D6(protected[p6] val x: String) {
    def x(): Int = 4 // err
  }
}

class D7(val x: String) {
  def x(): Int = 4 // err
}

class E1(x: String) {
  val x: Int = 4 // err
}

class E2(private[this] val x: String) {
  val x: Int = 4 // err
}

class E3(private val x: String) {
  val x: Int = 4 // err
}

object q4 {
  class E4(private[q4] val x: String) {
    val x: Int = 4 // err
  }
}

class E5(protected val x: String) {
  val x: Int = 4 // err
}

object q6 {
  class E6(protected[q6] val x: String) {
    val x: Int = 4 // err
  }
}

class E7(val x: String) {
  val x: Int = 4 // err
}
