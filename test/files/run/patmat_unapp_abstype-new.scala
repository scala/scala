import reflect.{ClassTag, classTag}

// abstract types and extractors, oh my!
trait TypesAPI {
  trait Type

  type TypeRef <: Type
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
      case MethodType(x) => println("MethodType")
      case _ => println("none of the above")
    }
  }
}

trait TypesImpl extends TypesAPI {
  object TypeRef extends TypeRefExtractor  // this will have a bridged unapply(x: Type) = unapply(x.asInstanceOf[TypeRef])
  case class TypeRef(n: Int) extends Type // this has a bridge from TypesAPI#Type to TypesImpl#TypeRef
  // --> the cast in the bridge will fail because the pattern matcher can't type test against the abstract types in TypesUser
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
    shouldNotCrash(TypeRef(10)) // prints "TypeRef"
    shouldNotCrash(MethodType(10)) // prints "MethodType"
  }
}

object TestUnappDynamicSynth extends RealFoos with Intermed {
 case class NotAFoo(n: Int) extends Bar
 def test() = {
   crash(NotAFoo(10))
   crash(new Foo(5))
 }
}

object Test extends App {
  TestUnappStaticallyKnownSynthetic.test()
  TestUnappDynamicSynth.test()
}
