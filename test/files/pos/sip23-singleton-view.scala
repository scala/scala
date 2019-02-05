import language.implicitConversions

class Test {
  implicit def singletonToString(c: Singleton): String = ""
  def foo(a: 1): String = a // implicit was being ruled out because Int(1).widen was not a subclass of Singletom
}
