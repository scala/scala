trait II {
  def apply(x: Int): Int
}

object Test {
  def ii(x: Int): Int = x
  def test = {
    val ii1: II = x => ii(x) // works
    val ii2: II = ii         // works (adapting `ii` to `II`)
    val ii3: II = ii _       // works (failed before the fix)
                             // typedTyped({ii : (() => <empty>)})
                             //    typedEta(ii, pt = II)
                             //       adapt(ii, pt = (? => ?))
                             //          instantiatedToMethodType(ii, pt = (? => ?))
    // val ii3: II = ii _ // error:
    // found   : Int => Int
    // required: II
  }
}