
/** Test which should fail compilation */
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





