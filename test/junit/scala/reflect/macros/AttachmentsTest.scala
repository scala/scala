package scala.reflect.macros

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.internal.util._

@RunWith(classOf[JUnit4])
class AttachmentsTest {
  def emptyAtt: Attachments = NoPosition

  @Test def testAttachments(): Unit = {
    var atts = emptyAtt

    assert(atts.isEmpty)
    assert(atts.all.isEmpty)

    atts = atts update Foo(0)
    assert(!atts.isEmpty)
    assert(atts.all == Set(Foo(0)))
    assert(atts.contains[Foo])
    assert(atts.get[Foo] == Some(Foo(0)))

    atts = atts.remove[Bar]
    assert(!atts.isEmpty)
    assert(atts.all == Set(Foo(0)))
    assert(atts.contains[Foo])
    assert(atts.get[Foo] == Some(Foo(0)))

    object theBar extends Bar
    atts = atts.update(theBar)
    assertFalse(atts.isEmpty)
    assertEquals(Set[Any](Foo(0), theBar), atts.all)
    assert(atts.get[Foo] == Some(Foo(0)))
    assert(atts.get[Bar] == Some(theBar))
    assert(atts.get[theBar.type] == Some(theBar))

    atts = atts.update(new Baz)
    assert(!atts.isEmpty)
    assert(!(atts.all contains Foo(0)))
    assert(atts.all.exists(_.isInstanceOf[Baz]))
    assert(atts.get[Baz].isDefined)
    assert(atts.get[Foo].isEmpty)
    assert(atts.get[Bar].isDefined)

    atts = atts.update("foo")
    atts = atts.update(Zzy)
    assert(!atts.isEmpty)
    assert(atts.all.size == 4, atts.toString)
    assert(atts.contains[String])
    assert(atts.contains[Zzy.type])
    assert(atts.contains[Baz])
    assert(atts.contains[theBar.type])

    atts = atts.remove[Zzy.type].remove[String]
    assert(atts.all.size == 2)
  }

}

case class Foo(i: Int) extends Baz
class Bar
class Baz
object Zzy
