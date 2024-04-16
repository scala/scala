//> using options -Werror -Xlint -Vdebug
//
// Multiple errors at a location are shown under debug.
//
class C {
  final def f: String = "hello, world"
}
class D extends C {
  override def f: Int = 42
}
