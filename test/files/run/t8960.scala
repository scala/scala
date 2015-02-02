object Test extends App {
  def test(o: AnyRef, sp: Boolean = false) = {
    val isSpecialized      = o.getClass.getSuperclass.getName contains "$sp"
    val isDelambdafyMethod = o.getClass.getName contains "$lambda$"
    assert(
      // delambdafy:method doesn't currently emit specialized anonymous function classes
      if (sp) (isSpecialized || isDelambdafyMethod) else !isSpecialized,
      o.getClass.getName)

    val Some(f) = o.getClass.getDeclaredFields.find(_.getName == "serialVersionUID")
    assert(f.getLong(null) == 0l)
  }

  test(() => (), sp = true)
  test(() => 1,  sp = true)
  test(() => "")

  test((x: Int) => x, sp = true)
  test((x: Boolean) => x)
  test((x: Int) => "")

  test((x1: Int, x2: Int) => 0d, sp = true)
  test((x1: Int, x2: AnyRef) => 0d)
  test((x1: Any, x2: Any) => x1)

  // scala> println((for (i <- 3 to 22) yield (for (j <- 1 to i) yield s"x$j: Int").mkString("  test((", ", ", ") => x1)")).mkString("\n"))

  test((x1: Int, x2: Int, x3: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int) => x1)
  test((x1: Int, x2: Int, x3: Int, x4: Int, x5: Int, x6: Int, x7: Int, x8: Int, x9: Int, x10: Int, x11: Int, x12: Int, x13: Int, x14: Int, x15: Int, x16: Int, x17: Int, x18: Int, x19: Int, x20: Int, x21: Int, x22: Int) => x1)

  test({
    case x: Int => x
  }: PartialFunction[Int, Int], sp = true)

  test({
    case x: Int => x
  }: PartialFunction[Any, Any])

  test({
    case x: Int => ()
  }: PartialFunction[Int, Unit], sp = true)

  test({
    case x: String => 1
  }: PartialFunction[String, Int])

  test({
    case x: String => ()
  }: PartialFunction[String, Unit])

  test({
    case x: String => x
  }: PartialFunction[String, String])
}
