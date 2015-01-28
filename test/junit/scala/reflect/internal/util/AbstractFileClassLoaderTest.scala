package scala.reflect.internal.util

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class AbstractFileClassLoaderTest {

  import scala.reflect.io._
  import scala.io.Source
  import scala.io.Codec.UTF8
  import scala.reflect.io.Streamable
  import java.net.{ URLClassLoader, URL }

  implicit def `we love utf8` = UTF8
  implicit class `abs file ops`(f: AbstractFile) {
    def writeContent(s: String): Unit = Streamable.closing(f.bufferedOutput)(os => os write s.getBytes(UTF8.charSet))
  }
  implicit class `url slurp`(url: URL) {
    def slurp(): String = Streamable.slurp(url)
  }

  val NoClassLoader: ClassLoader = null

  def fuzzBuzzBooz: (AbstractFile, AbstractFile) = {
    val fuzz = new VirtualDirectory("fuzz", None)
    val buzz = fuzz subdirectoryNamed "buzz"
    val booz = buzz fileNamed "booz.class"
    (fuzz, booz)
  }

  @Test
  def afclGetsParent(): Unit = {
    val p = new URLClassLoader(Array.empty[URL])
    val d = new VirtualDirectory("vd", None)
    val x = new AbstractFileClassLoader(d, p)
    assertSame(p, x.getParent)
  }

  @Test
  def afclGetsResource(): Unit = {
    val (fuzz, booz) = fuzzBuzzBooz
    booz writeContent "hello, world"
    val x = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val r = x.getResource("buzz/booz.class")
    assertNotNull(r)
    assertEquals("hello, world", r.slurp())
  }

  @Test
  def afclGetsResourceFromParent(): Unit = {
    val (fuzz, booz) = fuzzBuzzBooz
    val (fuzz_, booz_) = fuzzBuzzBooz
    booz writeContent "hello, world"
    booz_ writeContent "hello, world_"
    val p = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val x = new AbstractFileClassLoader(fuzz_, p)
    val r = x.getResource("buzz/booz.class")
    assertNotNull(r)
    assertEquals("hello, world", r.slurp())
  }

  @Test
  def afclGetsResourceInDefaultPackage(): Unit = {
    val fuzz = new VirtualDirectory("fuzz", None)
    val booz = fuzz fileNamed "booz.class"
    val bass = fuzz fileNamed "bass"
    booz writeContent "hello, world"
    bass writeContent "lo tone"
    val x = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val r = x.getResource("booz.class")
    assertNotNull(r)
    assertEquals("hello, world", r.slurp())
    assertEquals("lo tone", (x getResource "bass").slurp())
  }

  // SI-8843
  @Test
  def afclGetsResources(): Unit = {
    val (fuzz, booz) = fuzzBuzzBooz
    booz writeContent "hello, world"
    val x = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val e = x.getResources("buzz/booz.class")
    assertTrue(e.hasMoreElements)
    assertEquals("hello, world", e.nextElement.slurp())
    assertFalse(e.hasMoreElements)
  }

  @Test
  def afclGetsResourcesFromParent(): Unit = {
    val (fuzz, booz) = fuzzBuzzBooz
    val (fuzz_, booz_) = fuzzBuzzBooz
    booz writeContent "hello, world"
    booz_ writeContent "hello, world_"
    val p = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val x = new AbstractFileClassLoader(fuzz_, p)
    val e = x.getResources("buzz/booz.class")
    assertTrue(e.hasMoreElements)
    assertEquals("hello, world", e.nextElement.slurp())
    assertTrue(e.hasMoreElements)
    assertEquals("hello, world_", e.nextElement.slurp())
    assertFalse(e.hasMoreElements)
  }

  @Test
  def afclGetsResourceAsStream(): Unit = {
    val (fuzz, booz) = fuzzBuzzBooz
    booz writeContent "hello, world"
    val x = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val r = x.getResourceAsStream("buzz/booz.class")
    assertNotNull(r)
    assertEquals("hello, world", Streamable.closing(r)(is => Source.fromInputStream(is).mkString))
  }

  @Test
  def afclGetsClassBytes(): Unit = {
    val (fuzz, booz) = fuzzBuzzBooz
    booz writeContent "hello, world"
    val x = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val b = x.classBytes("buzz/booz.class")
    assertEquals("hello, world", new String(b, UTF8.charSet))
  }

  @Test
  def afclGetsClassBytesFromParent(): Unit = {
    val (fuzz, booz) = fuzzBuzzBooz
    val (fuzz_, booz_) = fuzzBuzzBooz
    booz writeContent "hello, world"
    booz_ writeContent "hello, world_"

    val p = new AbstractFileClassLoader(fuzz, NoClassLoader)
    val x = new AbstractFileClassLoader(fuzz_, p)
    val b = x.classBytes("buzz/booz.class")
    assertEquals("hello, world", new String(b, UTF8.charSet))
  }
}
