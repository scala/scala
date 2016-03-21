trait MySam { def apply(x: Int): String }

// check that SAM conversion happens after implicit view application
object Test extends App {
  final val AnonFunClass = "$anon$"
  final val LMFClass = "$$Lambda$" // LambdaMetaFactory names classes like this

  //  if there's an implicit conversion, it takes precedence
  def statusQuo() = {
    import language.implicitConversions
    var ok = false
    implicit def fun2sam(fun: Int => String): MySam = { ok = true; new MySam { def apply(x: Int) = fun(x) } }
    val className = (((x: Int) => x.toString): MySam).getClass.toString
    assert(ok, "implicit conversion not called")
    assert(className contains AnonFunClass, className)
    assert(!(className contains LMFClass), className)
  }

  // indirectly check that this sam type instance was created from a class spun up by LambdaMetaFactory
  def statusIndy() = {
    val className = (((x: Int) => x.toString): MySam).getClass.toString
    assert(!(className contains AnonFunClass), className)
    assert(className contains LMFClass, className)
  }

  statusQuo()
  statusIndy()
}