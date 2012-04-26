object Test extends App {
  // patterns in valdefs...
  // TODO: irrefutability should indicate we don't actually need to test, just deconstruct
  val (modified, result) : (Boolean, String) =  (true, null)
  println("meh"+ (modified, result))
}