package examples.mobile;

import java.net._;
import scala.mobile._;


object sort with Application {
  val url = new URL("http://scala.epfl.ch/classes/examples.jar");

  val location = new Location(url);
  val obj = location create "examples.sort";
  val ar = Array(6, 2, 8, 5, 1);
  obj[Array[Int], Unit]("println")(ar);
  obj[Array[Int], Unit]("sort")(ar);
  obj[Array[Int], Unit]("println")(ar);
  Console.println
}
