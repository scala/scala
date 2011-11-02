object Test extends Application {
  InnerClassTest1
  InnerClassTest2
}

object InnerClassTest1 extends Test1 {
  printClass(anonymousFunctions.getClass)
  printClass(anonymousFunctions.bar.getClass)
  println(anonymousClasses.x) // see run/t1167.scala
  printClass(anonymousClasses.getClass)
}

object InnerClassTest2 extends Test2 {
  printClass(anonymousFunctions.getClass)
  printClass(anonymousFunctions.bar.getClass)
  printClass(anonymousClasses.getClass)
  // not accessible via the Java reflection API
  printClass("anonymousFunctions$$anonfun$3")
  printClass("anonymousFunctions$$anonfun$foo$1")
  printClass("anonymousFunctions$bar$$anonfun$4")
  printClass("anonymousClasses$$anon$1")
}

object anonymousFunctions {
  //InnerClass:
  // public final #_ of #_; //class anonymousFunctions$$anonfun$1 of class InnerClass$
  val twice = (x: Int) => 2*x

  //InnerClass:
  // public final #_ of #_; //class anonymousFunctions$$anonfun$2
  List(0).map(x => x+1)

  def foo {
    //InnerClass:
    // public final #_ of #_; class anonymousFunctions$$anonfun$3
    val square = (x: Int) => x*x

    //InnerClass:
    // public final #_ of #_; class anonymousFunctions$$anonfun$foo$1
    Array(1).filter(_ % 2 == 0)
  }

  object bar {
    //InnerClass:
    // public final #_ of #_; class anonymousFunctions$bar$$anonfun$4 of class anonymousFunctions$bar$
    val cube = (x: Int) => x*x*x

    //InnerClass:
    // public final #_ of #_; class anonymousFunctions$bar$$anonfun$5
    Set(1, 2, 3).exists(_ == 2)
  }
}

object anonymousClasses {
  //InnerClass: 
  // public abstract #_= #_ of #_; //Foo=class anonymousClasses$Foo of class anonymousClasses$
  // public abstract #_= #_ of #_; //Foo$class=class anonymousClasses$Foo$class of class anonymousClasses$
  trait Foo {
    def foo() { println("foo"); }
    override def toString = getClass.getName
  }
  //InnerClass: 
  // public final #_; //class anonymousClasses$$anon$1 of class anonymousClasses$
  val x = new Foo() {
    override def foo() { println("foo (overriden)"); }
    def dummy = 0
  }
}

// Auxiliary functions

trait Test1 {
  private var kind: String = _
  private var mods: String = _
  def printInnerClasses(cls: Class[_]) {
    for (c <- cls.getDeclaredClasses) {
      mods = AccessFlags.asString(c.getModifiers)
      kind = if (c.isInterface) "interface" else "class"
      println("  "+mods+kind+" "+c.getName+
              " of class "+c.getEnclosingClass.getName)
    }
  }
  def printClass(cls: Class[_]) {
    println("\n{{ "+cls.getName+" }}")
    printInnerClasses(cls)
  }
}

trait Test2 {
  @throws(classOf[Exception])
  def printInnerClasses(cls: Class[_]) {
    import java.io._, ch.epfl.lamp.fjbg._
    val fjbgContext = new FJBGContext(49, 0)
    val outDir = System.getProperty("partest.output", "cf-attributes.obj")
    val fileName = outDir+File.separator+cls.getName+".class"
    val in = new DataInputStream(new FileInputStream(fileName))
    val jclass = fjbgContext.JClass(in)
    println(jclass.getInnerClasses)
    in.close()
  }
  def printClass(name: String) {
    try { printClass(Class.forName(name)) }
    catch { case e: Exception => println(e) }
  }
  def printClass(cls: Class[_]) {
    println("\n[[ "+cls.getName+" ]]");
    try { printInnerClasses(cls) }
    catch { case e: Exception => println(e) }    
  }
}

object AccessFlags {
  val ACC_PUBLIC    = 0x0001
  val ACC_PRIVATE   = 0x0002
  val ACC_PROTECTED = 0x0004
  val ACC_STATIC    = 0x0008
  val ACC_FINAL     = 0x0010
  val ACC_ABSTRACT  = 0x0400

  def asString(accessFlags: Int): String = {
    val buf = new StringBuilder()
    if ((accessFlags & ACC_PUBLIC) != 0) buf.append("public ")
    else if ((accessFlags & ACC_PROTECTED) != 0) buf.append("protected ")
    else if ((accessFlags & ACC_PRIVATE) != 0) buf.append("private ")
    if ((accessFlags & ACC_ABSTRACT) != 0) buf.append("abstract ")
    else if ((accessFlags & ACC_FINAL) != 0) buf.append("final ")
    buf.toString
  }
}

/*
  implicit def stringToLines(s: String) = new {
    def lines(n: Int): String = {
      val buf = new StringBuilder();
      var i = 0
      var from = 0
      while (i < n && 0 <= from && from < s.length) {
        val pos = s.indexOf('\n', from)
        if (pos >= 0) { i += 1; buf.append(s.substring(from, pos + 1)); }
        from = pos + 1
      }
      buf.toString()
    }
  }
*/

