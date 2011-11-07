/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/


import scala.concurrent._ 

object message {
   def main(args: Array[String]) = {
      val n = Integer.parseInt(args(0))
      val nActors = 500
      val finalSum = n * nActors

      case class Message(value: Int)

      class Incrementor(next: Pid) extends Actor {
         var sum = 0

         override def run() = {
            while (true) {
               receive { 
                  case Message(value) => 
                     val j = value + 1 
                     if (null != next){ 
                        next ! Message(j) 
                     } else { 
                        sum = sum + j
                        if (sum >= finalSum){ 
                           Console.println(sum); 
                           System.exit(0) // exit without cleaning up
                        }
                     } 
               }
            }
         }

         def pid() = { this.start; this.self }
      }

      def actorChain(i: Int, a: Pid): Pid = 
         if (i > 0) actorChain(i-1, new Incrementor(a).pid ) else a

      val firstActor = actorChain(nActors, null)
      var i = n; while (i > 0){ firstActor ! Message(0); i = i-1 }
   }
}
