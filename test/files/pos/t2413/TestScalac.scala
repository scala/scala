import pack.TestJava

class Foo extends TestJava {

   // THIS METHOD YIELDS TO CRASH
/*   def foomethod : Option[String] => Unit = {
      case None =>
        val path = repeatParam("s","a","b","c")
        ()
      case Some(error) =>
        ()
   }

  // THIS IS OK
  def foomethod2 : String = repeatParam("s","a");

  // THIS IS OK
  val aVal = repeatParam("1","2","3") */

  // THIS YIELDS TO CRASH
  for (a <- 1 to 4 ; anotherVal = repeatParam("1","2","3"))
    yield anotherVal
}
