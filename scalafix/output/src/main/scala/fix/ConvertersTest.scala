package fix

import strawman.collection.JavaConverters._
import strawman.collection.immutable.{ List, Map }

class ConvertersTest {
  def foo(xs: java.util.List[Int]): List[Int] = xs.asScala.toList
  def bar(xs: java.util.Map[Int, String]): Map[Int, String] = xs.asScala.toMap
  def baz(xs: List[Int]): java.util.List[Int] = xs.asJava
  def bah(xs: Map[Int, String]): java.util.Map[Int, String] = xs.asJava
}
