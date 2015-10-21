package scala.tools.nsc
package interpreter

import org.junit._, Assert._, runner.RunWith, runners.JUnit4

@RunWith(classOf[JUnit4])
class ScriptedTest {
  import javax.script._
  import scala.tools.nsc.interpreter.Scripted

  //new ScriptEngineManager().getEngineByName("scala").asInstanceOf[ScriptEngine with Compilable]
  def scripted = Scripted().asInstanceOf[ScriptEngine with Compilable]

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
    val code = """println("hello, world")"""

    ctx.setWriter(w)
    engine.eval(code, ctx)
    //assertEquals("hello, world", w.toString)
    ()
  }
}
