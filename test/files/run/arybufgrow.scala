import scala.collection.mutable._;

object Test extends App {
  val buf = new ArrayBuffer[String];
  for (i <- List.range(0,1000)) {
    buf += "hello";
  }

  Console.println("1000 = " + buf.length);
}
