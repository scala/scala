package tastytest

import collection.mutable

object TraitsWithSideEffects {

  private def setupEntries(map: mutable.Map[String, Boolean]): Unit = {
    map.addOne("True", true)
    map.addOne("False", false)
  }

  trait ExprMap {
    val map: mutable.Map[String, Boolean] = mutable.HashMap.empty

    setupEntries(map)
  }

  trait ExprMapNoField {
    val map: mutable.Map[String, Boolean]

    setupEntries(map)
  }

}
