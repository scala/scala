import jli._
import reflect.runtime

object Test extends App {
  val x = new SD_1.Q
  val y = new SD_2.Q

  println(M_1.testMacro(1)); println()
  println(M_1.testMacro(2)); println()
  // the output of this is befouled by scala/bug#7072
  print(M_1.test(1, runtime.universe)(runtime.currentMirror))

}
