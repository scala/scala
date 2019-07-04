package t11321

class X(val x: String) extends AnyVal
class Foo { def b = Option(new X("minnow")); def get = b.get }