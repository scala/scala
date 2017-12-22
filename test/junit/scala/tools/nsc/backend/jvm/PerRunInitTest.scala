package scala.tools.nsc.backend.jvm
import java.util

import org.junit._
import org.junit.Assert._

import scala.collection.mutable
import scala.ref.WeakReference
import scala.reflect.internal.util.JavaClearable
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.backend.jvm.PostProcessorFrontendAccess.PostProcessorFrontendAccessImpl
import scala.tools.nsc.reporters.StoreReporter

class PerRunInitTestMap extends PerRunInitTest {
  type Data =  mutable.Map[String, String]
  override def newData(): Data = underTest.recordPerRunCache(mutable.Map.empty)
  override def dontClear(data: Data): Unit = underTest.global.perRunCaches.unrecordCache(data)

  override def add(id: Int, data: Data): Unit = data.put(s"key $id", s"value $id")

  override def sizeOf(data: Data): Int = data.size

}
class PerRunInitTestSet extends PerRunInitTest {
  type Data =  mutable.Set[String]
  override def newData(): Data = underTest.recordPerRunCache(mutable.Set.empty)
  override def dontClear(data: Data): Unit = underTest.global.perRunCaches.unrecordCache(data)

  override def add(id: Int, data: Data): Unit = data += s"value $id"

  override def sizeOf(data: Data): Int = data.size
}
class PerRunInitTestJMap extends PerRunInitTest {
  type Data =  java.util.Map[String, String]
  override def newData(): Data = underTest.recordPerRunJavaMapCache(new util.HashMap[String,String]())
  override def dontClear(data: Data): Unit = underTest.global.perRunCaches.unrecordCache(JavaClearable.forMap(data))

  override def add(id: Int, data: Data): Unit = data.put(s"key $id", s"value $id")

  override def sizeOf(data: Data): Int = data.size
}
class PerRunInitTestJSet extends PerRunInitTest {
  type Data =  java.util.Set[String]
  override def newData(): Data = underTest.recordPerRunJavaCache(new util.HashSet[String]())
  override def dontClear(data: Data): Unit = underTest.global.perRunCaches.unrecordCache(JavaClearable.forCollection(data))

  override def add(id: Int, data: Data): Unit = data.add(s"value $id")

  override def sizeOf(data: Data): Int = data.size
}
class PerRunInitTestJCMap extends PerRunInitTestJMap {
  override def newData(): Data = underTest.recordPerRunJavaMapCache(new java.util.concurrent.ConcurrentHashMap[String,String]())
}
abstract class PerRunInitTest {
  type Data >: Null <: AnyRef
  var underTest : PostProcessorFrontendAccessImpl = _
  @Before def init() = {
    def global = {
      def showError(s: String) = throw new Exception(s)

      val settings = new Settings(showError)

      new Global(settings, new StoreReporter)
    }
    underTest = new PostProcessorFrontendAccessImpl(global)
  }
  @After def clear() = {
    underTest = null
  }

  def newData(): Data
  def dontClear(data:Data): Unit

  def add(id: Int, data: Data): Unit

  def sizeOf(data: Data): Int

  def clearCaches() = underTest.global.perRunCaches.clearAll()

  def doGc() = {
    System.gc()
    System.runFinalization()
  }

  @Test
  def clearedWhenExpired: Unit = {
    val data = newData()

    add(1, data)

    assertEquals(s"$data", 1, sizeOf(data))
    doGc()
    assertEquals(s"$data", 1, sizeOf(data))

    clearCaches()
    assertEquals(s"$data", 0, sizeOf(data))
  }

  @Test
  def clearedWeakOnly: Unit = {
    var data = newData()
    val ref = WeakReference(data)

    assertTrue(ref.get.isDefined)
    data = null
    doGc()
    assertFalse(ref.get.isDefined)
    //to check that dereference doesn't cause a problem
    clearCaches()
  }

  @Test
  def notClearedIfRequested: Unit = {
    val data = newData()
    dontClear(data)

    add(1, data)
    assertEquals(s"$data", 1, sizeOf(data))
    doGc()
    assertEquals(s"$data", 1, sizeOf(data))
    clearCaches()
    assertEquals(s"$data", 1, sizeOf(data))
  }



}
