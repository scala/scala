import scala.collection.mutable._
import scala.collection.script._


// #4461
object Test {
  def main(args: Array[String]) {
    val buf = new ArrayBuffer[Int] with ObservableBuffer[Int]
    buf.subscribe(new Subscriber[Message[Int], ObservableBuffer[Int]] {
      def notify(pub: ObservableBuffer[Int], event: Message[Int]) = println(event)
    })
    
    buf += 1 // works
    buf ++= Array(2) // works
    buf ++= ArrayBuffer(3, 4) // works
    buf ++= List(5) // works
    buf ++= collection.immutable.Vector(6, 7) // works
  }
}
