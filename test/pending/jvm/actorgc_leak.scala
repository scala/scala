
import scala.actors.Actor

object Test {
  class FatActorFactory extends Actor {
    def act() {
      var cnt = 0
      Actor.loopWhile(cnt < fatActors) {
	//if ((cnt % 5) == 0) println(cnt)
	val fa = new FatActor()
	fa.start()
	cnt += 1
	if (cnt == fatActors) Monitor ! 'done
      }
    }
  }
  
  class FatActor extends Actor {
    def act() {
      fat = new Array[Int](fatness)
      react {
	case 'hi => exit()
      }
    }
    private var fat: Array[Int] = _
  }

  object Monitor extends Actor {
    private var cnt = 0
    def act() {
      Actor.loop {
	react {
	  case 'done => {
	    cnt += 1
	    if (cnt == factories) System.exit(0) // once GC pressure stops FatActors stop being collected, and as
	  }                                     // a result ActorGC never finds out that they are defunct
	}
      }
    }
  }

  val factories = 4   // the number of factories to start
  val fatActors = 50  // the number of FatActors for each factory to produce
  val fatness = 1024*1024*10

  def main(args: Array[String]) {
    scala.actors.Scheduler.impl.shutdown()
    val sched = {
      val s = new scala.actors.FJTaskScheduler2
      s.start()
      s
    }
    scala.actors.Scheduler.impl = sched

    Monitor.start()
    for(i <- 1 to factories) {
      //if ((i % 50) == 0) println(i)
      val fa = new FatActorFactory()
      fa.start()
    }
    println("Done")
  }
}
