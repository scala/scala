import scala.actors._


class ReactorActor extends Reactor[String] {
   def act() {
     var cond = true
     loopWhile(cond) {
       react {
         case x if x == "hello1" => println(x.dropRight(1))
         case "exit" => cond = false
       }
     }
   }
}

class ReplyActor extends ReplyReactor {
   def act() {
     var cond = true
     loopWhile(cond) {
       react {
         case "hello" => println("hello")
         case "exit" => cond = false;
       }
     }
   }
}



object Test {

  def main(args: Array[String]) {
    val reactorActor = new ReactorActor
    val replyActor = new ReplyActor
    reactorActor.start()
    replyActor.start()

    reactorActor ! "hello1"
    replyActor ! "hello"

    reactorActor ! "exit"
    replyActor ! "exit"
  }
}
