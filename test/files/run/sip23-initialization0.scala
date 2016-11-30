class TestClazz  {
  def bar(x: "Hi"): x.type = x

  final val y = "Hi"

  val z0 = bar(y)
  assert(z0 == y)

  final val z1 = bar(y)
  assert(z1 == y)
}

object Test extends TestClazz with App
