package scala.tools.nsc
package interpreter

import org.junit._, Assert._, runner.RunWith, runners.JUnit4
import scala.tools.testing.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class ScriptedTest {
  import javax.script._
  import scala.tools.nsc.interpreter.Scripted

  def scripted: ScriptEngine with Compilable = Scripted()
    // same as by service discovery
    //new ScriptEngineManager().getEngineByName("scala").asInstanceOf[ScriptEngine with Compilable]

  // scripted, but also -Yno-predef -Yno-imports
  def scriptedNoNothing: ScriptEngine with Compilable = {
    val settings = new Settings()
    settings.noimports.value = true
    settings.nopredef.value = true
    Scripted(settings = settings)
  }

  @Test def eval() = {
    val engine = scripted
    engine.put("foo","bar")
    assert("bar" == engine.eval("foo"))
    val bindings = engine.createBindings()
    bindings.put("foo","baz")
    assert("baz" == engine.eval("foo", bindings))
    val c = engine.compile("def f = foo.asInstanceOf[String] ; f * 2")
    assert("barbar" == c.eval())
    assert("bazbaz" == c.eval(bindings))
  }
  @Test def evalNoNothing() = {
    val engine = scriptedNoNothing
    engine.put("foo","bar")
    assert("bar" == engine.eval("foo"))
    val bindings = engine.createBindings()
    bindings.put("foo","baz")
    assert("baz" == engine.eval("foo", bindings))
    val c = engine.compile("import scala.Predef.augmentString ; def f = foo.asInstanceOf[java.lang.String] ; f * 2")
    assert("barbar" == c.eval())
    assert("bazbaz" == c.eval(bindings))
  }
  @Test def `SI-7933 multiple eval compiled script`() = {
    val engine = scripted
    val init = """val i = new java.util.concurrent.atomic.AtomicInteger"""
    val code = """i.getAndIncrement()"""
    engine eval init
    val c = engine compile code
    assert(0 == c.eval())
    assert(1 == c.eval())
  }
  @Test def `SI-8422 captured i/o`() = {
    import java.io.StringWriter
    val engine = scripted
    val ctx    = new SimpleScriptContext
    val w      = new StringWriter
    val code = """print("hello, world")"""

    ctx.setWriter(w)
    engine.eval(code, ctx)
    assertEquals("hello, world", w.toString)
  }
  @Test def `SI-8422 captured multi i/o`() = {
    import java.io.{ StringWriter, StringReader }
    import scala.compat.Platform.EOL
    val engine = scripted
    val ctx    = new SimpleScriptContext
    val out    = new StringWriter
    val err    = new StringWriter
    val text   =
    """Now is the time
      |for all good
      |dogs to come for supper.""".stripMargin
    val in     = new StringReader(text)
        
    val code =
    """var s: String = _
      |var i: Int = 0
      |do {
      |  s = scala.io.StdIn.readLine()
      |  val out = if ((i & 1) == 0) Console.out else Console.err
      |  i += 1
      |  Option(s) foreach out.println
      |} while (s != null)""".stripMargin

    ctx.setWriter(out)
    ctx.setErrorWriter(err)
    ctx.setReader(in)
    engine.eval(code, ctx)
    val lines = text.lines.toList
    assertEquals(lines.head + EOL + lines.last + EOL, out.toString)
    assertEquals(lines(1) + EOL, err.toString)
  }
  @Test def `on compile error`(): Unit = {
    val engine = scripted
    val err = "not found: value foo in def f = foo at line number 11 at column number 16"
    assertThrows[ScriptException](engine.compile("def f = foo"), _ == err)
  }
}
