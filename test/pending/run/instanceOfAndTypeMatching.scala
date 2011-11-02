// Summary of incorrect or questionable behavior.
// Full code and successful parts follow.

object Summary {
  class Outer {
    class Inner { }
    def f() = { class MethodInner ; new MethodInner }
  }
  
  // 1 static issue:
  // 
  //   Given method in MethodInner: def g(other: MethodInner) = ()
  //   method1.g(method1) fails to compile with type error.
  //
  //   Note that this cannot be worked around by widening the return type
  //   of f() because MethodInner is declared inside of f.  So there is no way
  //   I see for a class declared inside a method to receive members of its
  //   own declared type -- not only the narrow type of those from this
  //   instance, but ANY members, because there is no Foo#Bar syntax which will
  //   traverse a method.
  //
  // 4 runtime issues:
  // 
  //   From the outside:     inner1.isInstanceOf[outer2.Inner] is true, should (maybe) be false
  //   From inside inner1:   inner2.isInstanceOf[Outer.this.Inner] is true, should (maybe) be false
  //   From the outside:     inner1 match { case _: outer2.Inner => true ... } is true, should definitely be false
  //   From inside method1:  method2 match { case _: MethodInner => true ... } is true, should definitely be false
  //
  //   Note that the fact that every test returns true on instances of MethodInner means
  //   that it is impossible to draw any type distinction between instances.  As far as one
  //   can tell, they are all of the same type regardless not only of whether they were
  //   created on the same method invocation but whether they are contained in the same
  //   instance of Outer.
  //
  //   WRT "same method invocation", see Iterator.duplicate for an example of this.
}

// Tests

class Outer {
  class Inner {
    def passOuter(other: Outer) = ()                  // pass any Outer
    def passThisType(other: Outer.this.type) = ()     // pass only this Outer instance
    def passInner(other: Inner) = ()                  // pass only Inners from this Outer instance
    def passInner2(other: Outer.this.Inner) = ()      // same as above
    def passInnerSharp(other: Outer#Inner) = ()       // pass any Inner
    
    def compareSimpleWithTypeMatch(other: Any) = other match {
      case _: Inner => true
      case _        => false
    }
    def compareSimpleWithInstanceOf(other: Any) = other.isInstanceOf[Inner]
    
    def compareSharpWithTypeMatch(other: Any) = {
      other match {
        case _: Outer#Inner => true
        case _              => false
      }
    }
    def compareSharpWithInstanceOf(other: Any) = other.isInstanceOf[Outer#Inner]
    
    def comparePathWithTypeMatch(other: Any) = other match {
      case _: Outer.this.Inner  => true
      case _                    => false
    }
    def comparePathWithInstanceOf(other: Any) = other.isInstanceOf[Outer.this.Inner]    
  }
  
  def f() = {
    class MethodInner { 
      def passOuter(other: Outer) = ()                  // pass any Outer
      def passThisType(other: Outer.this.type) = ()     // pass only this Outer instance
      def passInner(other: Inner) = ()                  // pass only Inners from this Outer instance
      def passInner2(other: Outer.this.Inner) = ()      // same as above
      def passInnerSharp(other: Outer#Inner) = ()       // pass any Inner
      def passMethodInner(other: MethodInner) = ()      // pass only MethodInners from this Outer instance
      // is there any way to refer to Outer#MethodInner? Not that there should be.
      
      def compareWithInstanceOf(other: Any) = other.isInstanceOf[MethodInner]
      def compareWithTypeMatch(other: Any) = other match {
        case _: MethodInner => true
        case _              => false
      }
    }
    
    new MethodInner
  }
}

object Test {
  val outer1 = new Outer
  val outer2 = new Outer
  val inner1 = new outer1.Inner
  val inner2 = new outer2.Inner
  val method1 = outer1.f()
  val method2 = outer2.f()
  
  def testInnerStatic = {
    // these should all work
    inner1.passOuter(outer1)
    inner1.passOuter(outer2)
    inner1.passThisType(outer1)
    inner1.passInner(inner1)
    inner1.passInner2(inner1)
    inner1.passInnerSharp(inner1)
    inner1.passInnerSharp(inner2)
    
    // these should all fail to compile, and do
    //
    // inner1.passThisType(outer2)
    // inner1.passInner(inner2)
    // inner1.passInner2(inner2)
  }
  def testInnerRuntime = {
    println("testInnerRuntime\n")
    
    List("These should be true under any scenario: ",
      inner1.isInstanceOf[outer1.Inner] , 
      inner1.isInstanceOf[Outer#Inner] ,
      (inner1: Any) match { case _: Outer#Inner => true ; case _ => false } ,
      (inner1: Any) match { case _: outer1.Inner => true ; case _ => false } ,
      inner1.compareSharpWithTypeMatch(inner2) ,
      inner1.compareSharpWithInstanceOf(inner2)
    ) foreach println
    
    List("These should be true under current proposal: ",
      inner1.compareSimpleWithInstanceOf(inner2) 
    ) foreach println
    
    List("These should be false under current proposal: ",
      inner1.compareSimpleWithTypeMatch(inner2) ,
      inner1.comparePathWithTypeMatch(inner2) 
    ) foreach println
    
    List("These return true but I think should return false: ", 
      inner1.isInstanceOf[outer2.Inner] ,               // true
      inner1.comparePathWithInstanceOf(inner2)          // true
    ) foreach println
    
    List("These are doing the wrong thing under current proposal",
      (inner1: Any) match { case _: outer2.Inner => true ; case _ => false }    // should be false
    ) foreach println
  }

  def testMethodInnerStatic = {
    // these should all work
    method1.passOuter(outer1)
    method1.passOuter(outer2)
    method1.passThisType(outer1)
    method1.passInner(inner1)
    method1.passInner2(inner1)
    method1.passInnerSharp(inner1)
    method1.passInnerSharp(inner2)
    // This fails with:
    //
    // a.scala:114: error: type mismatch;
    //  found   : Test.method1.type (with underlying type MethodInner forSome { type MethodInner <: java.lang.Object with ScalaObject{def passOuter(other: Outer): Unit; def passThisType(other: Test.outer1.type): Unit; def passInner(other: Test.outer1.Inner): Unit; def passInner2(other: Test.outer1.Inner): Unit; def passInnerSharp(other: Outer#Inner): Unit; def passMethodInner(other: MethodInner): Unit} })
    //  required: MethodInner where type MethodInner <: java.lang.Object with ScalaObject{def passOuter(other: Outer): Unit; def passThisType(other: Test.outer1.type): Unit; def passInner(other: Test.outer1.Inner): Unit; def passInner2(other: Test.outer1.Inner): Unit; def passInnerSharp(other: Outer#Inner): Unit; def passMethodInner(other: MethodInner): Unit}
    //     method1.passMethodInner(method1)
    //                             ^
    method1.passMethodInner(method1)
    
    // these should all fail to compile, and do
    //
    // method1.passThisType(outer2)
    // method1.passInner(inner2)
    // method1.passInner2(inner2)
    // method1.passMethodInner(method2)
  }
  
  def testMethodInnerRuntime = {
    println("\ntestMethodInnerRuntime\n")
    
    List("These should be true under any scenario: ",
      method1.compareWithInstanceOf(method1) ,
      method1.compareWithTypeMatch(method1) 
    ) foreach println
    
    List("These should be true under current proposal: ",
      method1.compareWithInstanceOf(method2)
    ) foreach println
    
    List("These are doing the wrong thing under current proposal",
      method1.compareWithTypeMatch(method2)    // should be false
    ) foreach println
  }
  
  def main(args: Array[String]): Unit = {
    testInnerRuntime
    testMethodInnerRuntime
  }
}
