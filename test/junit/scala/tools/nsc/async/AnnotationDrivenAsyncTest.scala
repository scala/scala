package scala.tools.nsc
package async

import java.io.File
import java.lang.reflect.InvocationTargetException
import java.nio.file.{Files, Paths}
import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.{Assert, Ignore, Test}

import scala.annotation.{StaticAnnotation, nowarn, unused}
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.reflect.internal.util.{CodeAction, Position}
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.tools.nsc.backend.jvm.AsmUtils
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.transform.TypingTransformers
import scala.tools.testkit.async.AsyncStateMachine

class AnnotationDrivenAsyncTest {
  @Test
  @Ignore // TODO XASYNC
  def testBoxedUnitNotImplemented(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.testkit.async.Async.{async, await}
        |import Future.successful
        |class A {
        |  def f = successful(this)
        |}
        |object Test {
        |  val data = List(("0", "0"))
        |  def test = async {
        |    val s1 = await(new A().f)
        |    s1.toString
        |    val s2 = await(s1.f)
        |    s2.toString
        |    val it = data.iterator
        |    while (it.hasNext) {
        |      val v = it.next()
        |      v match {
        |        case (x, y) =>
        |          "".isEmpty
        |          val r1 = await(s1.f).toString
        |          val r2 = await(s1.f).toString
        |          (r1, r2)
        |          val it = Nil.iterator
        |          while (it.hasNext) {
        |            val v = it.next()
        |            val r = await(s1.f).equals(v)
        |          }
        |      }
        |    }
        |  }
        |}
        |""".stripMargin
     assertEquals((), run(code))
  }

  @Test
  def testBasicScalaConcurrentCapture(): Unit = {
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.testkit.async.Async.{async, await}
        |
        |object Test {
        |  def test: Future[(String, Int, Int)] = async { var x = "init"; val y = await(f(1)); class C { x = x + "_updated" }; new C; (x, y, await(f(2))) }
        |  def f(x: Int): Future[Int] = Future.successful(x)
        |}
        |""".stripMargin
    assertEquals(("init_updated", 1, 2), run(code))
  }

  @Test
  def testScalaConcurrentAsyncNested(): Unit = {
    @nowarn("cat=lint-missing-interpolator")
    val code =
      """
        |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global
        |import scala.tools.testkit.async.Async.{async, await}
        |
        |object Test {
        |  def foo[T](a0: Int)(b0: Int*) = s"a0 = $a0, b0 = ${b0.head}"
        |
        |  def test: String = Await.result(async {
        |    var i = 0
        |    def get = async{i += 1; i}
        |    foo[Int](await(get))(await(get) :: await(async(Nil)) : _*)
        |  }, Duration.Inf)
        |}
        |""".stripMargin
    assertEquals("a0 = 1, b0 = 2", run(code))
  }

  @Test
  def testCustomAsync(): Unit = {
    val code = """
       |import scala.tools.nsc.async.{autoawait, customAsync}
       |
       |object Test {
       |  @customAsync
       |  def test: Any = {
       |    val x = reverse("abc")
       |    val y = reverse(x)
       |    (x, y)
       |  }
       |  @autoawait def reverse(a: String) = a.reverse
       |}
       |
       |""".stripMargin
    assertEquals(("cba", "abc"), run(code))
  }

  @Test
  def testMixedAsync(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |import scala.concurrent._, duration.Duration, ExecutionContext.Implicits.global, scala.tools.testkit.async.Async._
      |
      |object Test {
      |  @customAsync
      |  def test: Any = {
      |    class C {
      |      def repeat(s: String, i: Int): Future[String] = async {
      |        if (i == 0) s
      |        else await(repeat(s, i - 1)) + s
      |      }
      |    }

      |    val x = reverse("abc")
      |    val y = reverse(x)
      |    val z = Await.result(new C().repeat("-", 5), Duration.Inf)
      |    (x, y, z)
      |  }
      |  @autoawait def reverse(a: String) = a.reverse
      |}
      |
      |""".stripMargin
    assertEquals(("cba", "abc", "------"), run(code))
  }


  @Test
  def testExtractor(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test {
      |
      |  object Extractor1 {
      |    @autoawait def unapply(a: String) = Some((a + 1, a + 2))
      |  }
      |  object Extractor2 {
      |    @autoawait def unapply(a: String) = Some(a + 3)
      |  }
      |  @customAsync
      |  def test: Any = {
      |    @autoawait def id(a: String) = a
      |
      |    println("Test.test")
      |    val r1 = Predef.identity("blerg") match {
      |      case x if " ".isEmpty                                   => "case 2: " + x
      |      case Extractor1(Extractor2(x), y: String) if x == "xxx" => "case 1: " + x + ":" + y
      |        x match {
      |          case Extractor1(Extractor2(x), y: String) =>
      |          case _                                    =>
      |        }
      |      case Extractor2(x)                                      => "case 3: " + x
      |    }
      |    r1
      |  }
      |}
      |
      |""".stripMargin
    assertEquals("case 3: blerg3", run(code))
  }


  @Test
  def testLocalModule(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test {
      |  @customAsync def test: Any = {
      |    object Foo {
      |      @autoawait def id(a: String) = a
      |    }
      |    (Foo.id("a"), Foo.id("b"))
      |  }
      |}
      |
      |""".stripMargin
    assertEquals(("a", "b"), run(code))
  }

  @Test
  def testGuard(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test extends App {
      |  @customAsync
      |  def test: Any = {
      |    @autoawait def id[A](a: A) = a
      |
      |    "" match {
      |      case _ if id(false) => ???;
      |      case _              => id("okay")
      |    }
      |  }
      |}
      |
      |""".stripMargin
    assertEquals("okay", run(code))
  }


  @Test
  def testNestedMatchExtractor(): Unit = {
    val code = """
      |import scala.tools.nsc.async.{autoawait, customAsync}
      |
      |object Test extends App {
      |  @customAsync def test = {
      |    object Extractor {
      |      @autoawait def unapply(a: String) = Some((a, a))
      |    }
      |    "" match {
      |      case _ if "".isEmpty =>
      |        "" match {
      |          case Extractor(a, b) => a == b
      |        }
      |      case x               => throw new MatchError(x)
      |    }
      |  }
      |}
      |
      |""".stripMargin
    assertEquals(true, run(code))
  }

  @Test def testByNameOwner(): Unit = {
    @unused
    val result = run(
      """
      import scala.tools.nsc.async.{autoawait, customAsync}

      object Bleh {
        @autoawait def asyncCall(): Int = 0
        def byName[T](fn: => T): T = fn
      }
      object Boffo {
        @autoawait @customAsync def jerk(): Unit = {
          val pointlessSymbolOwner = 1 match {
            case _ =>
              Bleh.asyncCall()
              Bleh.byName {
                val whyDoHateMe = 1
                whyDoHateMe
              }
          }
        }
      }
      object Test {
        @customAsync def test() = Boffo.jerk()
      }
      """)
  }

  @Test def testByNameOwner2(): Unit = {
    @unused
    val result = run(
      """
      import scala.tools.nsc.async.{autoawait, customAsync}
      object Bleh {
        @autoawait def bleh = Bleh
        def byName[T](fn: => T): T = fn
      }
      object Boffo {
        @autoawait @customAsync def slob(): Unit = {
          val pointlessSymbolOwner =  {
              Bleh.bleh.byName {
                val whyDoHateMeToo = 1
                whyDoHateMeToo
              }
          }
        }
      }
      object Test {
        @customAsync def test() = Boffo.slob()
      }
      """)
  }

  @Test def testRewrittenApply(): Unit = {
    val result = run(
      """
        |import scala.tools.nsc.async.{autoawait, customAsync}
        |object O {
        |  case class Foo(a: Any)
        |}
        |object Test {
        |  @autoawait def id(a: String) = a
        |  @customAsync
        |  def test = {
        |    O.Foo
        |    id("foo") + id("bar")
        |    O.Foo(1)
        |  }
        |}
        | """.stripMargin)
    assertEquals("Foo(1)", result.toString)
  }

  @Test def testIsInstanceOfType(): Unit = {
    val result = run(
      """ import scala.tools.nsc.async.{autoawait, customAsync}
        |
        | class Outer
        | object Test {
        |   @autoawait def id(a: String) = a
        |   @customAsync def test = {
        |     val o = new Outer
        |     id("foo") + id("bar")
        |     ("": Object).isInstanceOf[o.type]
        |   }
        | }
        | """.stripMargin)
    assertEquals(false, result)
  }

  @Test def testIsInstanceOfTerm(): Unit = {
    val result = run(
      """import scala.tools.nsc.async.{autoawait, customAsync}
        |
        | class Outer
        | object Test {
        |   @autoawait def id(a: String) = a
        |   @customAsync def test = {
        |     val o = new Outer
        |     id("foo") + id("bar")
        |     o.isInstanceOf[Outer]
        |   }
        | }
        | """.stripMargin)
    assertEquals(true, result)
  }

  @Test def testArrayLocalModule(): Unit = {
    val result = run(
      """ import scala.tools.nsc.async.{autoawait, customAsync}
        |
        | class Outer
        | object Test {
        |   @autoawait def id(a: String) = a
        |   @customAsync def test = {
        |     val O = ""
        |     id("foo") + id("bar")
        |     new Array[O.type](0)
        |   }
        | }
        | """.stripMargin)
    assertEquals(classOf[Array[String]], result.getClass)
  }

  @Test
  def testPositions(): Unit = {
    val code =
      """
        |import scala.tools.nsc.async.{autoawait, customAsync}
        |object Test {
        |  @autoawait def id(a: Int) = a
        |  @customAsync def test = {
        |    val x = id(1) // L1
        |    val y = id(2) // L2
        |    x + y         // L3
        | }
        |}""".stripMargin

    val result = compile(code)

    import result.global._
    // settings.Xprintpos.value = true // enable to help debugging
    val fsmMethodParsed = result.parseTree.find { case dt: DefTree => dt.name.string_==("test") case _ => false }
    fsmMethodParsed.get match {
      case DefDef(_, _, _, _, _, Block(stats, expr)) =>
        // collect the two stats and expr from L1 L2 L3
        val parseTreeStats: List[Tree] = stats ++ List(expr)
        val fsmTree                        = result.fsmTree
        val posMap = mutable.LinkedHashMap[Tree, mutable.Buffer[Tree]]()

        // Traverse the tree and record parent-child relationship
        val parentMap = mutable.LinkedHashMap[Tree, Tree]()
        def collectParents(t: Tree): Unit = {
          for (child <- t.children) {
            parentMap(child) = t
            collectParents(child)
          }
        }
        def isAncestor(child: Tree, parent: Tree): Boolean = {
          parentMap.get(child) match {
            case None => false
            case Some(p) => parent == p || isAncestor(p, parent)
          }
        }
        collectParents(fsmTree.get)

        // Traverse the tree to find the genererated code the corresponds (by range position inclusion)
        // of the user-written stats/expr.
        for {
          parseTreeStat <- parseTreeStats
          pos = parseTreeStat.pos
          tree <- fsmTree.get
        } {
          if (pos.includes(tree.pos)) {
            posMap.get(parseTreeStat) match {
              case Some(existing) =>
                // Produce minimal output by discarding sub-trees that are contained by larger trees in the
                // value of posMap.
                if (!existing.exists(t => isAncestor(tree, t))) {
                  val (retained, discarded) = existing.toList.partition(t => isAncestor(t, tree) || !isAncestor(tree, t))
                  existing.clear()
                  existing ++= retained
                  existing += tree
                }
              case None =>
                posMap(parseTreeStat) = mutable.ListBuffer(tree)
            }
          }
        }

        // The synthetic exception handler and state machine loop should not be positioned within
        // the position of L1, L2, or L3, as this would trigger a breakpoint at the line on each
        // state transition
        val incorrectlyContainedTryOrWhileLoop = posMap.values.flatMap(_.collect { case t: Try => t; case ld: LabelDef if ld.name.containsName(nme.WHILE_PREFIX) => ld})
        assert(incorrectlyContainedTryOrWhileLoop.isEmpty, incorrectlyContainedTryOrWhileLoop)

        // Some synthetic code _will_ be at L1 L2 and L3, but this is only directly related to
        // the save and restore of the variables used/produced by this state.
        def oneliner(s: String) = s.replace(System.lineSeparator(), "\\n")
        val actual = posMap.toList.map { case (orig, corresponding) => s"${oneliner(orig.toString)}\n${"-" * 80}\n${corresponding.map(t => oneliner(t.toString)).mkString("\n")}"}.mkString("\n" * 3)
        val expected =
          """val x = id(1)
            |--------------------------------------------------------------------------------
            |case 0 => {\n  val awaitable$async: scala.tools.nsc.async.CustomFuture = scala.tools.nsc.async.CustomFuture._successful(scala.Int.box(Test.this.id(1)));\n  tr = self.getCompleted(awaitable$async);\n  self.state_=(1);\n  if (null.!=(tr))\n    while$()\n  else\n    {\n      self.onComplete(awaitable$async);\n      return ()\n    }\n}
            |<synthetic> val await$1: Object = {\n  val tryGetResult$async: Object = self.tryGet(tr);\n  if (self.eq(tryGetResult$async))\n    return ()\n  else\n    tryGetResult$async.$asInstanceOf[Object]()\n}
            |self.x = scala.Int.unbox(await$1)
            |
            |
            |val y = id(2)
            |--------------------------------------------------------------------------------
            |val awaitable$async: scala.tools.nsc.async.CustomFuture = scala.tools.nsc.async.CustomFuture._successful(scala.Int.box(Test.this.id(2)))
            |tr = self.getCompleted(awaitable$async)
            |self.state_=(2)
            |if (null.!=(tr))\n  while$()\nelse\n  {\n    self.onComplete(awaitable$async);\n    return ()\n  }
            |<synthetic> val await$2: Object = {\n  val tryGetResult$async: Object = self.tryGet(tr);\n  if (self.eq(tryGetResult$async))\n    return ()\n  else\n    tryGetResult$async.$asInstanceOf[Object]()\n}
            |val y: Int = scala.Int.unbox(await$2)
            |
            |
            |x.$plus(y)
            |--------------------------------------------------------------------------------
            |self.completeSuccess(scala.Int.box(self.x.+(y)))
            |return ()""".stripMargin
        assertEquals(expected, actual)
    }
  }


  // Handy to debug the compiler or to collect code coverage statistics in IntelliJ.
  @Test
  @Ignore
  def testManualRunPartestUnderJUnit(): Unit = {
    import scala.jdk.CollectionConverters._
    for (path <- List(Paths.get("../async/run"), Paths.get("../async/neg"))) {
      for (file <- Files.list(path).iterator.asScala) {
        if (file.getFileName.toString.endsWith(".scala")) {
          val code = new String(Files.readAllBytes(file))
          run(code, compileOnly = true)
        }
      }
    }
  }

  private def createTempDir(): File = {
    val f = File.createTempFile("output", "")
    f.delete()
    f.mkdirs()
    f
  }

  abstract class CompileResult {
    val global: Global
    val tree: global.Tree
    val parseTree: global.Tree
    def run(): Any
    def close(): Unit
    def fsmTree: Option[global.Tree] = tree.find { case dd: global.DefDef => dd.symbol.name.containsName("fsm"); case _ => false }
  }

  def run(code: String, compileOnly: Boolean = false): Any = {
    val compileResult = compile(code, compileOnly)
    try
      if (!compileOnly) compileResult.run()
    finally {
      compileResult.close()
    }
  }

  def compile(code: String, compileOnly: Boolean = false): CompileResult = {
    val out = createTempDir()

      val reporter = new StoreReporter(new Settings) {
        override def doReport(pos: Position, msg: String, severity: Severity, actions: List[CodeAction]): Unit =
          if (severity == INFO) println(msg)
          else super.doReport(pos, msg, severity, actions)
      }
      val settings = new Settings(println(_))
      settings.async.value = true
      settings.outdir.value = out.getAbsolutePath
      settings.Yrangepos.value = true
      settings.embeddedDefaults(getClass.getClassLoader)

    // settings.debug.value = true
    // settings.uniqid.value = true
    // settings.processArgumentString("-Xprint:typer,posterasure,async -nowarn")
    // settings.log.value = List("async")

    // NOTE: edit ANFTransform.traceAsync to `= true` to get additional diagnostic tracing.

    val isInSBT = !settings.classpath.isSetByUser
    if (isInSBT) settings.usejavacp.value = true
    val g = new Global(settings, reporter) {
      self =>

      @nowarn("cat=deprecation&msg=early initializers")
        object late extends {
          val global: self.type = self
        } with AnnotationDrivenAsyncPlugin

      override protected def loadPlugins(): List[Plugin] = late :: Nil
    }

    import g._

    val run    = new Run
    val source = newSourceFile(code)
    run.compileSources(source :: Nil)

    def showInfo(info: StoreReporter#Info): String = {
      Position.formatMessage(info.pos, info.severity.toString.toLowerCase + " : " + info.msg, shortenFile = false)
    }

    Assert.assertTrue(reporter.infos.map(showInfo).mkString("\n"), !reporter.hasErrors)
    Assert.assertTrue(reporter.infos.map(showInfo).mkString("\n"), !reporter.hasWarnings)

    val unit: CompilationUnit = run.units.next()
    val parseTree0 = newUnitParser(unit).parse()
    new CompileResult {
      val global: g.type = g

      val tree = unit.body
      override val parseTree: global.Tree = parseTree0

      def run(): Any = {
        try {
          val loader = new URLClassLoader(Seq(new File(settings.outdir.value).toURI.toURL), global.getClass.getClassLoader)
          val cls    = loader.loadClass("Test")
          val result = try {
            cls.getMethod("test").invoke(null)
          } catch {
            case ite: InvocationTargetException => throw ite.getCause
            case _: NoSuchMethodException       =>
              cls.getMethod("main", classOf[Array[String]]).invoke(null, null)
          }
          result match {
            case t: scala.concurrent.Future[_] =>
              scala.concurrent.Await.result(t, Duration.Inf)
            case cf: CustomFuture[_]           =>
              cf._block
            case cf: CompletableFuture[_]      =>
              cf.get()
            case value                         => value
          }
        } catch {
          case ve: VerifyError =>
            val asm = out.listFiles().flatMap { file =>
              val asmp = AsmUtils.textify(AsmUtils.readClass(file.getAbsolutePath))
              asmp :: Nil
            }.mkString("\n\n")
            throw new AssertionError(asm, ve)
        }
      }
      override def close(): Unit = {
        scala.reflect.io.Path.apply(out).deleteRecursively()
      }
    }
  }
}

abstract class AnnotationDrivenAsyncPlugin extends Plugin {

  import global._

  override val components: List[PluginComponent] = List(new PluginComponent with TypingTransformers {
    val global: AnnotationDrivenAsyncPlugin.this.global.type = AnnotationDrivenAsyncPlugin.this.global

    lazy val asyncModuleSym = symbolOf[CustomFuture.type]
    lazy val awaitSym = symbolOf[CustomFuture.type].info.member(TermName("_await"))
    lazy val autoAwaitSym = symbolOf[autoawait]
    lazy val customAsyncSym = symbolOf[customAsync]
    lazy val CustomFuture_class = symbolOf[CustomFuture.type]
    lazy val CustomFuture_successful = CustomFuture_class.companionModule.info.member(TermName("_successful"))

    def newTransformer(unit: CompilationUnit) = new TypingTransformer(unit) {
      override def transform(tree: Tree): Tree = {
        def wrapAwait = {
          localTyper.typedPos(tree.pos) {
            Apply(TypeApply(gen.mkAttributedRef(asyncModuleSym.typeOfThis, awaitSym), TypeTree(tree.tpe) :: Nil), gen.mkMethodCall(CustomFuture_successful, tree :: Nil) :: Nil)
          }
        }
        super.transform(tree) match {
          case ap@Apply(fun, _) if fun.symbol.hasAnnotation(autoAwaitSym) =>
            wrapAwait
          case sel@Select(_, _) if sel.symbol.hasAnnotation(autoAwaitSym) && !(tree.tpe.isInstanceOf[MethodType] || tree.tpe.isInstanceOf[PolyType]) =>
            wrapAwait
          case dd: DefDef if dd.symbol.hasAnnotation(customAsyncSym) =>
            deriveDefDef(dd) {
              rhs =>
                val fsmImplName = currentUnit.freshTermName("fsm$")
                val externalFsmMethod = true
                @nowarn("cat=lint-missing-interpolator")
                val name = TypeName("stateMachine$async")
                val wrapped = if (!externalFsmMethod) {
                  val applyMethod       =
                    q"""def apply(tr: _root_.scala.util.Either[_root_.scala.Throwable, _root_.scala.AnyRef]): _root_.scala.Unit = $rhs"""
                  val applyMethodMarked = global.async.markForAsyncTransform(dd.symbol, applyMethod, awaitSym, Map.empty)
                  q"""
                    class $name extends _root_.scala.tools.nsc.async.CustomFutureStateMachine {
                      $applyMethodMarked
                    }
                    new $name().start()
                   """
                } else {
                  val applyMethod       =
                    q"""def $fsmImplName(self: $name, tr: _root_.scala.util.Either[_root_.scala.Throwable, _root_.scala.AnyRef]): _root_.scala.Unit = $rhs"""
                  val applyMethodMarked = global.async.markForAsyncTransform(dd.symbol, applyMethod, awaitSym, Map.empty)
                  q"""
                    $applyMethodMarked
                    class $name extends _root_.scala.tools.nsc.async.CustomFutureStateMachine {
                       def apply(tr: _root_.scala.util.Either[_root_.scala.Throwable, _root_.scala.AnyRef]): _root_.scala.Unit =
                         $fsmImplName(this, tr)
                    }
                    new $name().start()
                   """
                }

                val tree =
                  q"""
                      val temp = ${wrapped}
                     temp._block
                    """
                val result = atOwner(dd.symbol) {
                  localTyper.typedPos(dd.pos) {
                    tree
                  }
                }
                result
            }
          case x => x
        }
      }
    }

    override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        newTransformer(unit).transformUnit(unit)
      }
    }

    override val runsAfter: List[String] = "refchecks" :: Nil
    override val runsRightAfter: Option[String] = Some("patmat")
    override val phaseName: String = "postpatmat"

  })
  override val description: String = "postpatmat"
  override val name: String = "postpatmat"
}

// Calls to methods with this annotation are translated to `Async.await(Future.successful(<call>))`
// This lets us express and test async boundaries in extractor calls, which one can't do with the async/await macro.
final class autoawait extends StaticAnnotation

final class customAsync extends StaticAnnotation

abstract class CustomFutureStateMachine extends AsyncStateMachine[CustomFuture[AnyRef], scala.util.Either[Throwable, AnyRef]] with Function1[scala.util.Either[Throwable, AnyRef], Unit] {
  private val result$async: CustomPromise[AnyRef] = new CustomPromise[AnyRef](scala.concurrent.Promise.apply[AnyRef]());
  private[this] var state$async: Int = 0
  protected def state: Int = state$async
  protected def state_=(s: Int): Unit = state$async = s
  def apply(tr$async: R[AnyRef]): Unit

  type F[A] = CustomFuture[A]
  type R[A] = Either[Throwable, A]
  // Adapter methods
  protected def completeFailure(t: Throwable): Unit = result$async._complete(Left(t))
  protected def completeSuccess(value: AnyRef): Unit = result$async._complete(Right(value))
  protected def onComplete(f: F[AnyRef]): Unit = f._onComplete(this)
  protected def getCompleted(f: F[AnyRef]): R[AnyRef] = f._getCompleted
  protected def tryGet(tr: R[AnyRef]): AnyRef = tr match {
    case Right(value) =>
      value
    case Left(throwable) =>
      result$async._complete(tr)
      this // sentinel value to indicate the dispatch loop should exit.
  }
  def start(): CustomFuture[AnyRef] = {
    CustomFuture._unit.asInstanceOf[CustomFuture[AnyRef]]._onComplete(this)
    result$async._future
  }
}
