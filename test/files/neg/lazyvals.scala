
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

object T2 {
  class A {
    val x: Int = { print("/*A.x*/"); 2 }
    lazy val y: Int = { print("/*A.y*/"); 2 }
  }

  
  class B extends A {
    // lazy overrides strict val
    override lazy val x: Int = { print("/*B.x*/"); 3 }
    // strict val overrides lazy
    override val y: Int = { print("/*B.y*/"); 3 }
  }
}





