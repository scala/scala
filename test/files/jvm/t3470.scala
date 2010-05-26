import scala.actors._

object Test {

  def expectActorState(a: Reactor[T] forSome { type T }, s: Actor.State.Value) {
    var done = false
    var i = 0
    while (!done) {
      i = i + 1
      if (i == 10) { // only wait for 2 seconds total
        println("FAIL ["+a+": expected "+s+"]")
        done = true
      }

      Thread.sleep(200)
      if (a.getState == s) // success
        done = true
    }
  }

  def main(args: Array[String]) {
    val a = new Actor { var c = 0; def act() = { c += 1; println("A: started: " + c) } }
    a.start()
    expectActorState(a, Actor.State.Terminated)
    a.restart()
    expectActorState(a, Actor.State.Terminated)
    a.restart()
  }

}
