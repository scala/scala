import scala.actors.Actor._

object Test {
  val rt = Runtime.getRuntime()
  val sender = actor {
    var cnt = 0
    while(cnt < 500) {
      if ((cnt % 100) == 0) println(cnt)
      receiver ! new Array[Int] (148576)
      cnt += 1
      //println ("Used Mem: " + (((rt.totalMemory() - rt.freeMemory()) / 1048576.) formatted "%.2f") + " Mb")
    }
    receiver ! 'exit
  }

  val receiver = actor {
    loop {
      react {
        case x: Array[Int] => ()//println ("received " + x.length)
	case 'exit => {
          println("done!")
	  exit()
	}
      }
    }
  }

  def main (args: Array[String]) {
    sender
  }
}
