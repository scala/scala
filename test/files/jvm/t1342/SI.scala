class SI extends JI {
   def varArgsMethod( args : String*) {
        for( arg <- args ) println( arg )
   }
}

object Test extends Application {
  val x: JI = new SI
  x.varArgsMethod("one", "two")
}
