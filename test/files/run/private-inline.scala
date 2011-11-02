
final class A {
  private var x1 = false
  var x2 = false
  
  // manipulates private var
  @inline private def wrapper1[T](body: => T): T = {
    val saved = x1
    x1 = true
    try body
    finally x1 = saved
  }
  // manipulates public var
  @inline private def wrapper2[T](body: => T): T = {
    val saved = x2
    x2 = true
    try body
    finally x2 = saved
  }
  
  // not inlined
  def f1a() = wrapper1(5)
  // inlined!
  def f1b() = identity(wrapper1(5))
  
  // not inlined
  def f2a() = wrapper2(5)
  // inlined!
  def f2b() = identity(wrapper2(5))  
}

object Test {
  def methodClasses = List("f1a", "f1b", "f2a", "f2b") map ("A$$anonfun$" + _ + "$1")
  
  def main(args: Array[String]): Unit = {
    val a = new A
    import a._
    println(f1a() + f1b() + f2a() + f2b())
    
    // Don't know how else to test this: all these should have been
    // inlined, so all should fail.
    methodClasses foreach { clazz =>
      
      val foundClass = (
        try   Class.forName(clazz)
        catch { case _ => null }
      )
      
      assert(foundClass == null, foundClass)
    }
  }
}
