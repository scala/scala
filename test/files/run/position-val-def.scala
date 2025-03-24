//> using options -Xsource:3-cross

import scala.reflect.internal.util.StringContextStripMarginOps
import scala.tools.partest.CompilerTest
import java.util.concurrent.atomic.AtomicInteger

object Test extends CompilerTest {
  import global.{show as tshow, *}

  val counter = new AtomicInteger

  override def sources =
    sm"""
    val x = 42
    var x = 42
    val x, y = 42
    var x, y = 42
    val (x, y) = (42, 27)
    val (x, y), (w, z) = (42, 27)
    val x, y, z: String = "hello, worlds"
    val Some(_) = Option(42)
    """.linesIterator.map(_.trim).filter(_.nonEmpty)
       .map(s => s"class C${counter.getAndIncrement} { $s }")
       .toList

  def check(source: String, unit: CompilationUnit): Unit = {
    println(source)
    //println("--")
    //println(tshow(unit.body))
    //println("--")
    unit.body.foreach {
      case t: ValOrDefDef if !t.symbol.isConstructor && !t.symbol.isParameter =>
        println(f"${tshow(t.namePos)}%-8s${tshow(t.pos)}%-8s${tshow(t.rhs.pos)}%-14s -> ${tshow(t).clipped}")
      case t: Assign =>
        println(f"${tshow(t.pos)}%-8s${tshow(t.rhs.pos)}%-22s -> ${tshow(t).clipped}")
      case _ =>
    }
    println("--")
  }
  implicit class Clippy(val s: String) extends AnyVal {
    def clipped = {
      val it = s.linesIterator
      val t = it.next()
      if (it.hasNext) s"$t..." else t
    }
  }
}
