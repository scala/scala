object Test extends App {
  def testAnonFunClass(o: AnyRef, sp: Boolean = false) = {
    val isSpecialized      = o.getClass.getSuperclass.getName contains "$sp"
    val isDelambdafyMethod = o.getClass.getName contains "$lambda$"
    assert(
      // delambdafy:method doesn't currently emit specialized anonymous function classes
      if (sp) (isSpecialized || isDelambdafyMethod) else !isSpecialized,
      o.getClass.getName)

    val Some(f) = o.getClass.getDeclaredFields.find(_.getName == "serialVersionUID")
    assert(f.getLong(null) == 0l)
  }

  def testIndyLambda(o: AnyRef, sp: Boolean = false) = {
    val isSpecialized = o.getClass.getInterfaces.exists(_.getName contains "$sp")
    assert(sp == isSpecialized, o.getClass.getName)
  }


  testIndyLambda(() => (), sp = true)
  testIndyLambda(() => 1,  sp = true)
  testIndyLambda(() => "")

  testIndyLambda((x: Int) => x, sp = true)
  testIndyLambda((x: Boolean) => x)
  testIndyLambda((x: Int) => "")

  testIndyLambda((x1: Int, x2: Int) => 0d, sp = true)
  testIndyLambda((x1: Int, x2: AnyRef) => 0d)
  testIndyLambda((x1: Any, x2: Any) => x1)

  // scala> println((for (i <- 3 to 22) yield (for (j <- 1 to i) yield s"x$j: Int").mkString("  testIndyLambda((", ", ", ") => x1)")).mkString("\n"))

  testIndyLambda((x1: Int, x2: Int, x3: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int) => x1)
  testIndyLambda((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int, x22: Int) => x1)

  testAnonFunClass({
    case x: Int => x
  }: PartialFunction[Int, Int], sp = true)

  testAnonFunClass({
    case x: Int => x
  }: PartialFunction[Any, Any])

  testAnonFunClass({
    case x: Int => ()
  }: PartialFunction[Int, Unit], sp = true)

  testAnonFunClass({
    case x: String => 1
  }: PartialFunction[String, Int])

  testAnonFunClass({
    case x: String => ()
  }: PartialFunction[String, Unit])

  testAnonFunClass({
    case x: String => x
  }: PartialFunction[String, String])
}
