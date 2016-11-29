object Test {
  def main(args: Array[String]): Unit = {
    val a: Array[String] = "goobledy bing, goobledy bling, wikka wokka wup.".split("")
    val b = java.util.Arrays.asList(a: _*)
    java.util.Collections.shuffle(b)

    // we'll say rather unlikely a.sameElements(b) unless
    // they are pointing to the same array
    import scala.collection.convert.ImplicitConversionsToScala._
    assert(a sameElements b)
  }
}
