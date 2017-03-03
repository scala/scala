trait MySam { def apply(x: Int): String }

// check that SAM conversion happens after implicit view application
object Test extends App {
  final val AnonFunClass = "$anon$"
  final val LMFClass = "$$Lambda$" // LambdaMetaFactory names classes like this

  //  if there's an implicit conversion, it does not takes precedence (because that's what dotty does)
  def implicitSam() = {
    import language.implicitConversions
    var ok = true
    implicit def fun2sam(fun: Int => String): MySam = { ok = false; new MySam { def apply(x: Int) = fun(x) } }
    val className = (((x: Int) => x.toString): MySam).getClass.toString
    assert(ok, "implicit conversion not called")
    assert(!(className contains AnonFunClass), className)
    assert(className contains LMFClass, className)
  }

  // indirectly check that this sam type instance was created from a class spun up by LambdaMetaFactory
  def justSammy() = {
    val className = (((x: Int) => x.toString): MySam).getClass.toString
    assert(!(className contains AnonFunClass), className)
    assert(className contains LMFClass, className)
  }

  implicitSam()
  justSammy()
}
