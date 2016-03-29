trait T[@specialized A] { def apply(a: A): A }
trait TInt extends T[Int]

// Check that we expand the SAM of a type that is specialized.
// This is an implementation restriction -- the current specialization scheme is not
// amenable to using LambdaMetaFactory to spin up subclasses.
// Since the generic method is abstract, and the specialized ones are concrete,
// specialization is rendered moot because we cannot implement the specialized method
// with the lambda using LMF.
object Test extends App {
  final val AnonFunClass = "$anonfun$"
  final val LMFClass = "$$Lambda$" // LambdaMetaFactory names classes like this

  def specializedSamPrecludesLMF() = {
    val className = ((x => x): T[Int]).getClass.toString
    assert((className contains AnonFunClass), className)
    assert(!(className contains LMFClass), className)
  }

  def specializedSamSubclassPrecludesLMF() = {
    val className = ((x => x): TInt).getClass.toString
    assert((className contains AnonFunClass), className)
    assert(!(className contains LMFClass), className)
  }

  def nonSpecializedSamUsesLMF() = {
    val className = ((x => x): T[String]).getClass.toString
    assert(!(className contains AnonFunClass), className)
    assert(className contains LMFClass, className)
  }

  specializedSamPrecludesLMF()
  nonSpecializedSamUsesLMF()
}
