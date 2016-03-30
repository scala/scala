trait T[@specialized A] { def apply(a: A): A }
trait TInt extends T[Int]

trait TWithVal { val x: Any = 1; def apply(x: Int): String }

trait TImpure { def apply(x: Int): String ; println(1) }

trait Println { println(1) }
trait TImpureSuper extends Println { def apply(x: Int): String  }

class C
trait A extends C
trait B extends A
trait TClassParent extends B { def apply(x: Int): String }

object Test extends App {
  final val AnonFunClass = "$anonfun$"
  final val LMFClass = "$$Lambda$" // LambdaMetaFactory names classes like this

  private def LMF(f: Any): Unit = {
    val className = f.getClass.toString
    assert(!(className contains AnonFunClass), className)
    assert((className contains LMFClass), className)
  }

  private def notLMF(f: Any): Unit = {
    val className = f.getClass.toString
    assert((className contains AnonFunClass), className)
    assert(!(className contains LMFClass), className)
  }

  // Check that we expand the SAM of a type that is specialized.
  // This is an implementation restriction -- the current specialization scheme is not
  // amenable to using LambdaMetaFactory to spin up subclasses.
  // Since the generic method is abstract, and the specialized ones are concrete,
  // specialization is rendered moot because we cannot implement the specialized method
  // with the lambda using LMF.

  // not LMF if specialized at this type
  notLMF((x => x): T[Int])
  // not LMF if specialized at this type (via subclass)
  notLMF((x => x): TInt)
  // LMF ok if not specialized at this type
  LMF((x => x): T[String])

  // traits with a val member also cannot be instantiated by LMF
  val fVal: TWithVal = (x => "a")
  notLMF(fVal)
  assert(fVal.x == 1)

  notLMF((x => "a"): TImpure)
  notLMF((x => "a"): TImpureSuper)

  val fClassParent: TClassParent = x => "a"
  notLMF(fClassParent)
  assert(fClassParent(1) == "a")
}
