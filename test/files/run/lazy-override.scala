class A {
  val x: Int = { print("/*A.x*/"); 2 }
  lazy val y: Int = { print("/*A.y*/"); 2 }
  lazy val z: Int = { print("/*A.z*/"); 2 }
}

class B extends A {
  // lazy overrides strict val
  override lazy val x: Int = { print("/*B.x*/"); 3 }
  // strict val overrides lazy
  override val y: Int = { print("/*B.y*/"); 3 }
  // lazy overrides lazy
  override lazy val z: Int = { print("/*B.z/"); 3 }
}




object Test extends Application {
 val a = new A
 print("a.x=")
 println(a.x)

 val b = new B
 print("b.x=")
 println(b.x)
 print("b.z=")
 println(b.z)
}
