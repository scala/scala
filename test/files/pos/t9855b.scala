object Test {
  var FALSE = false
  def main(args: Array[String]): Unit = {
    val SomeB = new B
    new B() match {
      case SomeB if FALSE =>
      case SomeB =>
      case Ext(_) =>
    }
  }
}
object Ext {
  def unapply(s: A) = Some(())
}
class A
class B extends A
