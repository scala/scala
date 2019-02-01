
@identity case class InteropIdentity(x: Int)
@placebo case class InteropPlacebo(x: Int)

object Test extends App {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  // @Test def caseModuleSynthesisForIdentity: Unit =
  {
    assertEquals(InteropIdentity.toString, "InteropIdentity")
  }

  // @Test def caseModuleSynthetisForPlacebo: Unit =
  {
    assertEquals(InteropPlacebo.toString, "InteropPlacebo")
  }
}
