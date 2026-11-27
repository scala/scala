package scala.tools.reflect

import java.lang.reflect.InvocationTargetException

import scala.collection.mutable
import scala.reflect.internal.Reporter.{ERROR, WARNING}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testkit.BytecodeTesting

/** Compiles f-interpolations with the compiler under test and runs those that compile.
 *
 *  All expressions of a call are compiled in one run, each on its own line of one object, so that
 *  thousands of cases take seconds.
 */
object FormatInterpolatorTesting {
  /** A message of the compiler, at `column`, an index into the expression, or -1 if it has no position. */
  final case class Message(error: Boolean, column: Int, text: String) {
    override def toString = s"${if (error) "error" else "warning"} at $column: $text"
  }

  /** What compiling and running an expression gave. `result` is empty if there was an error. */
  final case class Outcome(expr: String, messages: List[Message], result: Option[Either[Throwable, String]]) {
    def errors   = messages.filter(_.error)
    def warnings = messages.filterNot(_.error)
    override def toString = s"$expr: ${messages.mkString("[", "; ", "]")} ${result.fold("")(_.fold(e => s"threw $e", r => s"= \"$r\""))}"
  }

  /** Values that the expressions can refer to. */
  val Decls: String =
    """val s = "Scala"
      |val d = 8
      |val b = false
      |val f = 3.14159
      |val c = 'c'
      |val t = new java.util.Date(0L)
      |val i = 42
      |val any: Any = 7
      |val anyVal: AnyVal = 7
      |val number: Number = Integer.valueOf(7)
      |val date = java.time.LocalDate.of(2012, 5, 26)
      |val temporal: java.time.temporal.TemporalAccessor = date
      |val x = new java.util.Formattable {
      |  def formatTo(ff: java.util.Formatter, g: Int, w: Int, p: Int): Unit = ff.format("xxx")
      |}""".stripMargin

  private val compilers = mutable.Map.empty[String, scala.tools.testkit.Compiler]
  // report all messages, there may be many
  private def compiler(options: String) =
    compilers.getOrElseUpdate(options, BytecodeTesting.newCompiler(extraArgs = s"-Xmaxerrs 1000000 -Xmaxwarns 1000000 $options"))

  private val Prefix = "  def m"

  // the source, and for each line of it the case and the offset in its expression of the line's first char
  private def source(exprs: Seq[(String, Int)], decls: String): (String, Map[Int, (Int, Int)]) = {
    val header = s"object FTest {\n$decls\n"
    val lines = mutable.Map.empty[Int, (Int, Int)]
    var line = header.count(_ == '\n') + 1
    for ((e, k) <- exprs) {
      val prefix = s"$Prefix$k: Any = "
      var offset = -prefix.length
      for (l <- e.split("\n", -1)) {
        lines(line) = (k, offset)
        offset += (if (offset < 0) l.length + prefix.length else l.length) + 1
        line += 1
      }
    }
    val code = exprs.map { case (e, k) => s"$Prefix$k: Any = $e" }.mkString(header, "\n", "\n}\n")
    (code, lines.toMap)
  }

  /** Compile `exprs`, under `compileLocale` as the default locale if given, and run those that compile. */
  def run(exprs: Seq[String], decls: String = Decls, options: String = "", compileLocale: Option[java.util.Locale] = None): Seq[Outcome] = {
    val comp = compiler(options)
    import comp.global
    val indexed = exprs.zipWithIndex

    // compile everything, and keep the messages of each line
    def compile(cases: Seq[(String, Int)]): (Map[Int, List[Message]], List[(String, Array[Byte])]) = {
      val (code, lines) = source(cases, decls)
      val run = comp.newRun()
      val locale = java.util.Locale.getDefault
      compileLocale.foreach(java.util.Locale.setDefault)
      try run.compileSources(List(BytecodeTesting.makeSourceFile(code, "FTest.scala")))
      finally java.util.Locale.setDefault(locale)
      val reporter = global.reporter.asInstanceOf[StoreReporter]
      val byLine = mutable.Map.empty[Int, List[Message]].withDefaultValue(Nil)
      for (info <- reporter.infos.toList if info.severity == ERROR || info.severity == WARNING) {
        val pos = info.pos
        val (k, column) =
          if (!pos.isDefined) (-1, -1)
          else lines.get(pos.line) match {
            case Some((k, offset)) => (k, offset + pos.column - 1)
            case None              => (-2, -1) // not in an expression
          }
        byLine(k) = byLine(k) :+ Message(info.severity == ERROR, column, info.msg)
      }
      val classes =
        if (reporter.hasErrors) Nil
        else BytecodeTesting.getGeneratedClassfiles(global.settings.outputDirs.getSingleOutput.get)
      (byLine.toMap.withDefaultValue(Nil), classes)
    }

    val (messages, _) = compile(indexed)
    val unexplained = messages(-2)
    assert(unexplained.isEmpty, s"messages not on the line of an expression: $unexplained")
    assert(messages(-1).isEmpty || exprs.length == 1, s"messages without a position, in a run of several cases: ${messages(-1)}")
    val ok = indexed.filter { case (_, k) => !messages(k).exists(_.error) }
    val results: Map[Int, Either[Throwable, String]] =
      if (ok.isEmpty) Map.empty
      else {
        val (again, classes) = compile(ok)
        assert(again.values.forall(!_.exists(_.error)), s"errors when compiling again: $again")
        val loader = new ClassLoader(getClass.getClassLoader) {
          private val bytes = classes.map { case (name, b) => name.stripSuffix(".class").replace('/', '.') -> b }.toMap
          override def findClass(name: String): Class[_] = bytes.get(name) match {
            case Some(b) => defineClass(name, b, 0, b.length)
            case None    => throw new ClassNotFoundException(name)
          }
        }
        val cls = loader.loadClass("FTest$")
        val module = cls.getField("MODULE$").get(null)
        ok.map { case (_, k) =>
          val r =
            try Right(String.valueOf(cls.getMethod(s"m$k").invoke(module)))
            catch { case e: InvocationTargetException => Left(e.getCause) }
          k -> r
        }.toMap
      }
    // with a single case, a message without a position is its own
    indexed.map { case (e, k) => Outcome(e, messages(k) ++ (if (exprs.length == 1) messages(-1) else Nil), results.get(k)) }
  }
}
