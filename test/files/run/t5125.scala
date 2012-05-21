object O1 {
  def instance = this
  @scala.annotation.varargs
  def f(values:String*) = println("Calling O1.f(): " + values)
}

object O2 {
  def instance = this
  @scala.annotation.varargs
  def f(values:String*) = println("Calling O2.f(): " + values)
  // uncommenting g() results in errors in A.java
  def g(): String => Int = s => s.hashCode
}

object Test extends App {
  def check(c: Class[_]) {
    val methodName = "f"
    val methods = c.getDeclaredMethods.filter(_.getName == methodName)
    println(methods.map(_.toString).sorted.mkString("\n"))
  }

  check(O1.getClass)
  check(O2.getClass)
}