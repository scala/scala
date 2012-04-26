import scala.tools.nsc.interactive._
import tests._

object Test extends InteractiveTest {
	val Reps = 30
	import compiler._

	def askSomething(): Response[Tree] = {
		// println("*")
		Thread.sleep(50)
		ask { compiler.askStructure(true)(sourceFiles.head, _) }
	}

	def fireAsks() {
		val jobs1 = for (i <- 1 until Reps) yield {
			if (i % 10 == 0) {
				askReload(sourceFiles)
			}
			askSomething
		} 

		for ((j, i) <- jobs1.zipWithIndex) {
			j.get(40000) match {
				case None => 
					println(i + ": TIMEOUT")
					exit(1) // no need to delay the test any longer
				case r => 
			}
		}
		compiler.askShutdown()

		println("No timeouts")
	}
	
	override def main(args: Array[String]) {
		new Thread("Asking") {
			override def run() {
				fireAsks()
			}
		}.start()

		Thread.sleep(800)
		compiler.askShutdown()
	}
}