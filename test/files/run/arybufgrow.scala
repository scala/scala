import scala.collection.mutable._;

object Test extends Application {
  val buf = new ArrayBuffer[String];
  for(val i <- List.range(0,1000)) {
    buf + "hello";
  }

  Console.println("1000 = " + buf.length);
}
