import scala.actors._, scala.actors.Actor._, scala.actors.Futures._

object Test {

  def main(args: Array[String]) {

    val a = actor {
      loop { react {
        case i: Int => reply(i * 2)
        case 'stop => exit()
      } }
    }

    val fts = for (_ <- 1 to 1000)
      yield a !! (3, {case x: Int => x})

    def respondAll(fts: List[Future[Int]], cnt: Int): Unit =
      fts match {
        case List() => a ! 'stop
        case ft :: rest =>
          if (cnt % 100 == 0)
            println(ft())
          respondAll(rest, cnt + 1)
      }

    actor {
      respondAll(fts.toList, 0)
    }

  }

}
