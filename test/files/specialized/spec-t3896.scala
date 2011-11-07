// see ticket #3896. Tests interaction between overloading, specialization and default params
trait Atomic[@specialized(Boolean) T] {
  def x: T
  
  // crash depends on the overloading: if second method is "g", no crash.
  def f(fn: T => T): Boolean = f(fn(x))
  def f[R](a: T, b: R = true) = b
}
class AtomicBoolean(val x: Boolean) extends Atomic[Boolean]

object Test {
  def main(args: Array[String]): Unit = {
    val e = new AtomicBoolean(false)
    val x = e.f( (a : Boolean) => !a ) // ok
    println( e.f( (a : Boolean) => !a ) toString ) // ok
    println( e.f( (a : Boolean) => !a) ) // compiler crash
    
    println(runtime.BoxesRunTime.integerBoxCount)
  }
}
