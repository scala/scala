// abstract types and extractors, oh my!
trait TypesAPI {
  trait Type

  // an alternative fix (implemented in the virtual pattern matcher, is to replace the isInstanceOf by a manifest-based run-time test)
  // that's what typeRefMani is for
  type TypeRef <: Type //; implicit def typeRefMani: Manifest[TypeRef]
  val TypeRef: TypeRefExtractor; trait TypeRefExtractor {
    def apply(x: Int): TypeRef
    def unapply(x: TypeRef): Option[(Int)]
  }

  // just for illustration, should follow the same pattern as TypeRef
  case class MethodType(n: Int) extends Type
}

// user should not be exposed to the implementation
trait TypesUser extends TypesAPI {
  def shouldNotCrash(tp: Type): Unit = {
    tp match {
      case TypeRef(x) => println("TypeRef") 
      // the above checks tp.isInstanceOf[TypeRef], which is erased to tp.isInstanceOf[Type]
      //   before calling TypeRef.unapply(tp), which will then crash unless tp.isInstanceOf[TypesImpl#TypeRef] (which is not implied by tp.isInstanceOf[Type])
      // tp.isInstanceOf[TypesImpl#TypeRef] is equivalent to classOf[TypesImpl#TypeRef].isAssignableFrom(tp.getClass)
      // this is equivalent to manifest
      // it is NOT equivalent to manifest[Type] <:< typeRefMani
      case MethodType(x) => println("MethodType")
      case _ => println("none of the above")
    }
  }
}

trait TypesImpl extends TypesAPI {
  object TypeRef extends TypeRefExtractor  // this will have a bridged unapply(x: Type) = unapply(x.asInstanceOf[TypeRef])
  case class TypeRef(n: Int) extends Type // this has a bridge from TypesAPI#Type to TypesImpl#TypeRef 
  // --> the cast in the bridge will fail because the pattern matcher can't type test against the abstract types in TypesUser
  //lazy val typeRefMani = manifest[TypeRef]
}

trait Foos {
 trait Bar
 type Foo <: Bar
 trait FooExtractor {
   def unapply(foo: Foo): Option[Int]
 }
 val Foo: FooExtractor
}

trait RealFoos extends Foos {
 class Foo(val x: Int) extends Bar
 object Foo extends FooExtractor {
   def unapply(foo: Foo): Option[Int] = Some(foo.x)
 }
}

trait Intermed extends Foos {
 def crash(bar: Bar): Unit =
   bar match {
     case Foo(x) => println("Foo")
     case _ => println("Bar")
   }
}

object TestUnappStaticallyKnownSynthetic extends TypesImpl with TypesUser {
  def test() = {
    shouldNotCrash(TypeRef(10)) // should and does print "TypeRef"
    // once  #1697/#2337 are fixed, this should generate the correct output
    shouldNotCrash(MethodType(10)) // should print "MethodType" but prints "none of the above" -- good one, pattern matcher!
  }
}

object TestUnappDynamicSynth extends RealFoos with Intermed {
 case class FooToo(n: Int) extends Bar
 def test() = {
   crash(FooToo(10))
   crash(new Foo(5))
 }
}

object Test extends App {
  TestUnappStaticallyKnownSynthetic.test()
  TestUnappDynamicSynth.test()
}
