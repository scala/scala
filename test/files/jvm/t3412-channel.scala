import scala.actors._, scala.actors.Actor._, scala.actors.Futures._

object Test {

  def main(args: Array[String]) {

    actor {
      val C: Channel[Int] = new Channel[Int](self)

      def respondAll(fts: List[Future[Int]], cnt: Int): Unit =
        fts match {
          case List() => C ! 0
          case ft :: rest =>
            if (cnt % 100 == 0)
              println(ft())
            respondAll(rest, cnt + 1)
        }

      actor {
        val fts = for (_ <- 1 to 1000)
                  yield C !! (3, {case x: Int => x})

        actor {
          respondAll(fts.toList, 0)
        }
      }

      loop {
        C.react {
          case 0 => exit()
          case i => reply(i * 2)
        }
      }
    }

  }

}
