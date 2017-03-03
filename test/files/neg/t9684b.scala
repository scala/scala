trait T1 {
  import scala.collection.JavaConverters._
  import scala.collection.JavaConversions._

  null.asInstanceOf[java.util.Iterator[String]]: Iterator[String] // works
  asScalaIterator(null)                                           // fails: asScalaIterator is imported twice.
}

trait T2 {
  import scala.collection.JavaConversions.asScalaIterator

  null.asInstanceOf[java.util.Iterator[String]]: Iterator[String] // works
  asScalaIterator(null)                                           // works
}
