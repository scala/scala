import collection.{convert, mutable, concurrent, JavaConverters}
import convert.ImplicitConversionsToScala._
import java.util.concurrent.{ConcurrentHashMap => CHM}

object Foo {
  def buildCache2_9_simple[K <: AnyRef, V <: AnyRef]: concurrent.Map[K, V] =
    JavaConverters.mapAsScalaConcurrentMap(new CHM())

  def buildCache2_9_implicit[K <: AnyRef, V <: AnyRef]: concurrent.Map[K, V] =
    new CHM[K, V]()
}

object Bar {
  def assertType[T](t: T) = t
  val a = new CHM[String, String]() += (("", ""))
  assertType[concurrent.Map[String, String]](a)
}
