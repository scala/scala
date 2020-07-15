package tastytest

import collection.mutable

trait TraitWithSideEffects {
  val map: mutable.AnyRefMap[String, Boolean] = mutable.AnyRefMap.empty

  map.addOne("True", true)
  map.addOne("False", false)
}

object TraitWithSideEffects {

  val example = new TraitWithSideEffects {}

  assert(verify(example))

  def verify(t: TraitWithSideEffects): Boolean = {
    val theMap = t.getClass().getField("map")
    theMap.setAccessible(true)
    val map = theMap.get(t).asInstanceOf[t.map.type]
    map.getOrElse("True", false) && !map.getOrElse("False", true)
  }
}
