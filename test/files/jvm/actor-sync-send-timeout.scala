import scala.actors.Actor

/* This test is a regression test for SI-4759.
 */
object Test { 
  val Runs = 5
  
  def main(args: Array[String]) = {
    var i = 0
    while (i < Runs) {
      i += 1
      A1 ! 1
      Thread.sleep(500)
    }
    //println("done sending to A1")
  }
}

object A2 extends Actor {
  this.start()
  def act() {
    loop {
      react {
        case 'stop =>
          //println("A2 exiting")
          exit()
        case _ =>
      }
    }
  }
}

object A1 extends Actor {
  this.start()
  def act() {
    var i = 0
    loopWhile(i < Test.Runs) {
      i += 1
      react {
        case any =>
          A2 !? (500, any)
          if (i == Test.Runs)
            A2 ! 'stop
      }
    }
  }
}
