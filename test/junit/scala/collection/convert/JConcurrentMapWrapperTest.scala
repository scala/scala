package scala.collection.convert

import org.junit.Test

import java.util.concurrent.{ConcurrentHashMap, ConcurrentSkipListMap}

import scala.collection.concurrent.ConcurrentMapTestHelper
import scala.jdk.CollectionConverters._

class JConcurrentMapWrapperTest {
  @Test
  def CHM_filterInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_filterInPlace(new ConcurrentHashMap[String, Int].asScala)
  }

  @Test
  def CHM_mapValuesInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_mapValuesInPlace(new ConcurrentHashMap[String, Int].asScala)
  }

  @Test
  def CSLM_filterInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_filterInPlace(new ConcurrentSkipListMap[String, Int].asScala)
  }

  @Test
  def CSLM_mapValuesInPlace(): Unit = {
    ConcurrentMapTestHelper.genericTest_mapValuesInPlace(new ConcurrentSkipListMap[String, Int].asScala)
  }
}
