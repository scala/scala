class C1 {
  @scala.annotation.varargs
  def f(values:String*) = println("Calling C1.f(): "  + values)
}

class C2 {
  @scala.annotation.varargs
  def f(values:String*) = println("Calling C2.f(): "  + values)
  def g(): String => Int = s => s.hashCode

  class C3 {
    @scala.annotation.varargs
    def f(values:String*) = println("Calling C3.f(): "  + values)
  }
}

class C4 {
  def f(values: String*) = println("Calling C4.f(): " + values)

  locally {
    @scala.annotation.varargs
    def f(values: String*) = println("Calling C4.<locally>.f(): " + values)
  }
}

class C5 {
  def f(values: String*) = println("Calling C5.f(): " + values)
  @scala.annotation.varargs
  private def f(v: Int, values: Int*) = println("Calling C5.f(): " + values)

  def method(): Unit = {
    @scala.annotation.varargs
    def f(values: String*) = println("Calling C5.<locally>.f(): " + values)
  }
}

object Test extends App {
  def check(c: Class[_]) {
    val methodName = "f"
    val methods = c.getDeclaredMethods.filter(_.getName == methodName)
    println(methods.map(_.toString).sorted.mkString("\n"))
  }

  check(classOf[C1])
  check(classOf[C2])
  check(classOf[C2#C3])
  check(classOf[C4])
  check(classOf[C5])
}
