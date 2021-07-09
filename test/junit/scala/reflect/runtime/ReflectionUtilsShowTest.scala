package scala.reflect.runtime

import java.net.{URL, URLClassLoader}

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ReflectionUtilsShowTest {
  @Test def testGetUrlsCanReturnNull(): Unit = {
    val sut = new MyClassLoader(Array.empty[URL])
    assert(ReflectionUtils.show(sut).contains("<unknown>"))
  }
}

class MyClassLoader(urls: Array[URL]) extends URLClassLoader(urls) {
  override def getURLs: Array[URL] = null
}
