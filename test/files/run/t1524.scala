object Test extends Application {

  val buf = new scala.collection.mutable.ArrayBuffer[String](0)
  buf += "initial"
  buf += "second"
  println(buf.head)
}
