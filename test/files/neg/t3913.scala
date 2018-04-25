class Stage( val transits: Set[ Stage ])
object LimboStage  extends Stage( Set( LimboStage ))

object Test {
   def main( args: Array[ String ]): Unit = {
      val x = LimboStage
	   }
}
