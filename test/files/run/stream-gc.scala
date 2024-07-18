//> using javaOpt -Xmx5M -Xms5M

import scala.collection.immutable._

object Test extends App {
  Stream.tabulate(100)(_ => new Array[AnyRef](10000)).find(_ => false)
  Stream.tabulate(100)(_ => new Array[AnyRef](10000)).collect { case x if false => x }
  Stream.tabulate(100)(_ => new Array[AnyRef](10000)).collectFirst { case x if false => x }
  Stream.tabulate(100)(_ => new Array[AnyRef](10000)).collectFirst { case x if false => x }
  Stream.tabulate(100)(_ => new Array[AnyRef](10000)).collectFirst { case x if false => x }
  Stream.tabulate(100)(_ => new Array[AnyRef](10000)).iterator.foreach(_ => ())
}
