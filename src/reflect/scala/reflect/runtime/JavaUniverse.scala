package scala
package reflect
package runtime

import scala.reflect.internal.{TreeInfo, SomePhase}
import scala.reflect.internal.{SymbolTable => InternalSymbolTable}
import scala.reflect.runtime.{SymbolTable => RuntimeSymbolTable}
import scala.reflect.api.{TypeCreator, Universe}

/** An implementation of [[scala.reflect.api.Universe]] for runtime reflection using JVM classloaders.
 *
 *  Should not be instantiated directly, use [[scala.reflect.runtime.universe]] instead.
 *
 *  @contentDiagram hideNodes "*Api" "*Extractor"
 */
class JavaUniverse extends InternalSymbolTable with JavaUniverseForce with ReflectSetup with RuntimeSymbolTable { self =>
  def picklerPhase = SomePhase
  def erasurePhase = SomePhase
  lazy val settings = new Settings

  private val isLogging = sys.props contains "scala.debug.reflect"
  def log(msg: => AnyRef): Unit = if (isLogging) Console.err.println("[reflect] " + msg)

  // TODO: why put output under isLogging? Calls to inform are already conditional on debug/verbose/...
  import scala.reflect.internal.{Reporter, ReporterImpl}
  override def reporter: Reporter = new ReporterImpl {
    protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = log(msg)
  }

  // minimal Run to get Reporting wired
  def currentRun = new RunReporting {}
  class PerRunReporting extends PerRunReportingBase {
    def deprecationWarning(pos: Position, msg: String, since: String): Unit = reporter.warning(pos, msg)
  }
  protected def PerRunReporting = new PerRunReporting


  type TreeCopier = InternalTreeCopierOps
  implicit val TreeCopierTag: ClassTag[TreeCopier] = ClassTag[TreeCopier](classOf[TreeCopier])
  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier

  def currentFreshNameCreator = globalFreshNameCreator

  override lazy val internal: Internal = new SymbolTableInternal {
    override def typeTagToManifest[T: ClassTag](mirror0: Any, tag: Universe # TypeTag[T]): Manifest[T] = {
      // SI-6239: make this conversion more precise
      val mirror = mirror0.asInstanceOf[Mirror]
      val runtimeClass = mirror.runtimeClass(tag.in(mirror).tpe)
      Manifest.classType(runtimeClass).asInstanceOf[Manifest[T]]
    }
    override def manifestToTypeTag[T](mirror0: Any, manifest: Manifest[T]): Universe # TypeTag[T] =
      TypeTag(mirror0.asInstanceOf[Mirror], new TypeCreator {
        def apply[U <: Universe with Singleton](mirror: scala.reflect.api.Mirror[U]): U # Type = {
          mirror.universe match {
            case ju: JavaUniverse =>
              val jm = mirror.asInstanceOf[ju.Mirror]
              val sym = jm.classSymbol(manifest.runtimeClass)
              val tpe =
                if (manifest.typeArguments.isEmpty) sym.toType
                else {
                  val tags = manifest.typeArguments map (targ => ju.internal.manifestToTypeTag(jm, targ))
                  ju.appliedType(sym.toTypeConstructor, tags map (_.in(jm).tpe))
                }
              tpe.asInstanceOf[U # Type]
            case u =>
              u.internal.manifestToTypeTag(mirror.asInstanceOf[u.Mirror], manifest).in(mirror).tpe
          }
        }
      })
  }

  // can't put this in runtime.Trees since that's mixed with Global in ReflectGlobal, which has the definition from internal.Trees
  object treeInfo extends {
    val global: JavaUniverse.this.type = JavaUniverse.this
  } with TreeInfo

  init()

  // ======= Initialization of runtime reflection =======
  //
  // This doc describes the carefully laid out sequence of actions used to initialize reflective universes.
  //
  // Before reading the text below, read up the section Mirrors in the reflection pre-SIP
  // https://docs.google.com/document/d/1nAwSw4TmMplsIlzh2shYLUJ5mVh3wndDa1Zm1H6an9A/edit.
  // Take an especially good look at Figure 2, because it illustrates fundamental principles underlying runtime reflection:
  //   1) For each universe we have one mirror per classloader
  //   2) Package symbols are per-mirror
  //   3) Other symbols are per-universe, which means that a symbol (e.g. Seq on the picture) might be shared between multiple owners
  //
  // Main challenges that runtime reflection presents wrt initialization are:
  //   1) Extravagant completion scheme that enters package members on-demand rather than a result of scanning a directory with class files.
  //      (That's a direct consequence of the fact that in general case we can't enumerate all classes in a classloader.
  //      As Paul rightfully mentioned, we could specialcase classloaders that point to filesystems, but that is left for future work).
  //   2) Presence of synthetic symbols that aren't loaded by normal means (from classfiles) but are synthesized on-the-fly,
  //      and the necessity to propagate these synthetic symbols from rootMirror to other mirrors,
  //      complicated by the fact that such symbols depend on normal symbols (e.g. AnyRef depends on Object).
  //   3) Necessity to remain thread-safe, which limits our options related to lazy initialization
  //      (E.g. we cannot use missingHook to enter synthetic symbols, because that's thread-unsafe).
  //
  // Directly addressing the challenge #3, we create all synthetic symbols fully in advance during init().
  // However, it's not that simple as just calling definitions.symbolsNotPresentInBytecode.
  // Before doing that, we need to first initialize ObjectClass, then ScalaPackageClass, and only then deal with synthetics.
  // Below you can find a detailed explanation for that.
  //
  // ### Why ScalaPackageClass? ###
  //
  // Forcing ScalaPackageClass first thing during startup is important, because syntheticCoreClasses such as AnyRefClass
  // need to be entered into ScalaPackageClass, which entails calling ScalaPackageClass.info.decls.enter.
  // If ScalaPackageClass isn't initialized by that moment, the following will happen for runtime reflection:
  //   1) Initialization of ScalaPackageClass will trigger unpickling.
  //   2) Unpickling will need to load some auxiliary types such as, for example, String.
  //   3) To load String, runtime reflection will call mirrorDefining(classOf[String]).
  //   4) This, in turn, will call runtimeMirror(classOf[String].getClassLoader).
  //   5) For some classloader configurations, the resulting mirror will be different from rootMirror.
  //   6) In that case, initialization of the resulting mirror will try to import definitions.syntheticCoreClasses into the mirror.
  //   7) This will force all the lazy vals corresponding to syntheticCoreClasses.
  //   8) By that time, the completer of ScalaPackageClass will have already called setInfo on ScalaPackageClass, so there won't be any stack overflow.
  //
  // So far so good, no crashes, no problems, right? Not quite.
  // If forcing of ScalaPackageClass was called by a syntheticCoreClasses lazy val,
  // then this lazy val will be entered twice: once during step 7 and once when returning from the original call.
  // To avoid this we need to initialize ScalaPackageClass prior to other synthetics.
  //
  // ### Why ObjectClass? ###
  //
  // 1) As explained in JavaMirrors.missingHook, initialization of ScalaPackageClass critically depends on AnyRefClass.
  // 2) AnyRefClass is defined as "lazy val AnyRefClass = newAlias(ScalaPackageClass, tpnme.AnyRef, ObjectTpe)",
  //    which means that initialization of AnyRefClass depends on ObjectClass.
  // 3) ObjectClass is defined as "lazy val ObjectClass = getRequiredClass(sn.Object.toString)",
  //    which means that under some classloader configurations (see JavaMirrors.missingHook for more details)
  //    dereferencing ObjectClass might trigger an avalanche of initializations calling back into AnyRefClass
  //    while another AnyRefClass initializer is still on stack.
  // 4) That will lead to AnyRefClass being entered two times (once when the recursive call returns and once when the original one returns)
  // 5) That will crash PackageScope.enter that helpfully detects double-enters.
  //
  // Therefore, before initializing ScalaPackageClass, we must pre-initialize ObjectClass
  def init() {
    definitions.init()

    // workaround for http://groups.google.com/group/scala-internals/browse_thread/thread/97840ba4fd37b52e
    // constructors are by definition single-threaded, so we initialize all lazy vals (and local object) in advance
    // in order to avoid deadlocks later (e.g. one thread holds a global reflection lock and waits for definitions.Something to initialize,
    // whereas another thread holds a definitions.Something initialization lock and needs a global reflection lock to complete the initialization)

    // TODO Convert this into a macro
    force()
  }
}
