
import scala.tools.nsc._
import interpreter.ILoop

object Test {

  def main(args : Array[String]) {

    val code = """
  val n = 2
  () => n
    """

    val s = new Settings()
    s.optimise.value = false
    s.debug.value  = true
    s.log.value = List("all")
    val lines = ILoop.runForTranscript(code + "\n" + code, s).lines.toList


    if (lines exists (_ contains "Abandoning crashed session")) {
     lines foreach println
    	println("crashed!")
    } else {
      println("completed successfully")
    }
  }
}

