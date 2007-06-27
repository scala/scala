
/** Test which should fail compilation */
class Lazy {

  // no abstract lazy values
  lazy val t: Int

  // no lazy var
  lazy var p: Int = 100

  // no lazy defs
  lazy def q: Double = 0.0

  def foo {
    lazy val t;
    ()
  }

  // no trait/class/object can be lazy
  lazy trait T {}
  lazy class C {}
  lazy object O {}

  // no lazy modifiers in class parameters
  class A(lazy val obj: Object) {}
}
