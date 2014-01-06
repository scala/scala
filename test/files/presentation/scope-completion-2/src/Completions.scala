package test

/* private elements are visible in the companion class/object */

class Completion1 {

  import Completion1._

  private val vc1 = 0
  private def fc1 = 0

  private class Cc1 {
  }

  /*_*/
}

object Completion1 {

  val c = new Completion1()
  import c._

  private val vo1 = 0
  private def fo1 = 0

  private class Co1 {
  }

  /*_*/
}

