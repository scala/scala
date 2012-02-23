
/** Test which should fail compilation */
class ElysianFailed {

  import ElysianField._

  // fine
  val a: Int = myInt

  // fine
  unimplemented()

  // not fine
  val b: Nothing = unimplemented()
  
}

object ElysianField {

  import annotation.elidable

  @elidable(100) def unimplemented(): Nothing = throw new UnsupportedOperationException

  @elidable(100) def myInt: Int = 17

}





