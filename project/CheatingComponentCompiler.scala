import sbt._

import xsbti.{Logger => _,_}
import compiler._
import inc._
import Locate.DefinesClass
import java.io.File

import java.util.concurrent._
import Keys._
import Compiler.Compilers


object CheatingCompilerSettings {
  val hackedmodules = TaskKey[Map[String,File]]("hacked-sbt-components")

  def settings: Seq[Setting[_]] = Seq(
    hackedmodules := Map.empty,

    // Now look for org.scala-sbt compiler-interface-src!
    hackedmodules <<= (hackedmodules, target, streams) map { (h, t, s) => Map(sbtCompilerInterface(t,s)) ++ h },

    compilers <<= (hackedmodules, scalaInstance, appConfiguration, streams, classpathOptions, javaHome) map { 
      (hacks, si, app, s, co, jh) => CheatingCompilers.compilers(hacks, si, co, jh)(app, s.log) 
    }
  )


  def sbtCompilerInterface(cacheDir: File, s: TaskStreams): (String, File) =
    "compiler-interface-src" -> pullFile("compiler-interface-src", "http://typesafe.artifactoryonline.com/typesafe/ivy-releases/org.scala-sbt/compiler-interface/0.12.0-M2/jars/compiler-interface-src.jar", cacheDir, s)

  import dispatch.{url=>_,_}
  def pullFile(name: String, url: String, cacheDir: File, s: TaskStreams): File = {
    val cachedFile = cacheDir / (name + ".jar")
    if (!cachedFile.exists) {
      // Ensure the directory for the cache exists.
      cachedFile.getParentFile.mkdirs()
      val fous = new java.io.FileOutputStream(cachedFile)
      try Http(dispatch.url(url) >>> fous) finally fous.close()
      val sha = pullSha(url)
      if(!isShaOk(cachedFile, sha)) sys.error("Downloaded sha does not match file: " + cachedFile.getAbsolutePath)
      IO.write(file(cachedFile.getAbsolutePath + ".sha1"), sha)
    }
    cachedFile
  }

  def isShaOk(f: File, sha: String): Boolean = 
     ShaResolve.calculateSha(f) == sha

  def isCached(f: File, url: String): Boolean = 
    if(f.exists) {
      val savedsha = file(f.getAbsolutePath + ".sha1")
      savedsha.exists && isShaOk(f, IO.read(savedsha))
    } else false

  def pullSha(url: String): String = 
    Http(dispatch.url(url + ".sha1") >- identity)
}


object CheatingCompilers {

  def compilers(hacks: Map[String,File], instance: ScalaInstance, cpOptions: ClasspathOptions, javaHome: Option[File])(implicit app: AppConfiguration, log: Logger): Compilers = {
    val javac = Compiler.directOrFork(instance, cpOptions, javaHome)
    val scalac = scalaCompiler(hacks, instance, cpOptions)
    new Compilers(scalac, javac)
  }

  def scalaCompiler(hacks: Map[String,File], instance: ScalaInstance, cpOptions: ClasspathOptions)(implicit app: AppConfiguration, log: Logger): AnalyzingCompiler = {
    val launcher = app.provider.scalaProvider.launcher
    val componentManager = new CheatingComponentManager(hacks, launcher.globalLock, app.provider.components, Option(launcher.ivyHome), log)
    new AnalyzingCompiler(instance, componentManager, cpOptions, log)
  }
}


class CheatingComponentManager(val hacks: Map[String,File], globalLock: xsbti.GlobalLock, provider: xsbti.ComponentProvider, ivyHome: Option[File], log: Logger) 
  extends ComponentManager(globalLock, provider, ivyHome, log) {
  
  private[this] val ivyCache = new IvyCache(ivyHome)
  /** Get all of the files for component 'id', throwing an exception if no files exist for the component. */
  override def files(id: String)(ifMissing: IfMissing): Iterable[File] = {
    def fromGlobal1 =
      lockGlobalCache {
        try { update(id); getOrElse1(createAndCache1) }
        catch { case e: NotInCache => createAndCache1 }
      }
    def getOrElse1(orElse: => Iterable[File]): Iterable[File] = {
      val existing = provider.component(hacked(id))
      if(existing.isEmpty) orElse else existing
    }
    def notFound1 = invalid("Could not find required component '" + id + "'")
    def createAndCache1 =
      ifMissing match {
        case IfMissing.Fail      => notFound1
        case d: IfMissing.Define =>
          d()
          if(d.cache) cache(id)
          getOrElse1(notFound1)
      }
    val result = lockLocalCache { getOrElse1(fromGlobal1) }
    println("cheating-component-manager.files("+id+")")
    result foreach println
    result
  }
  private def isHacked(id: String) = hacks.keySet contains id
  private def hacked(id: String) = if(isHacked(id)) "hacked-"+id else id
  /** This is used to lock the local cache in project/boot/.  By checking the local cache first, we can avoid grabbing a global lock. */
  private def lockLocalCache[T](action: => T): T = lock(provider.lockFile)( action )
  /** This is used to ensure atomic access to components in the global Ivy cache.*/
  private def lockGlobalCache[T](action: => T): T = lock(ivyCache.lockFile)( action )
  private def lock[T](file: File)(action: => T): T = globalLock(file, new Callable[T] { def call = action })
  private def invalid(msg: String) = throw new InvalidComponent(msg)
  private def invalid(e: NotInCache) = throw new InvalidComponent(e.getMessage, e)

  override def define(id: String, files: Iterable[File]) = lockLocalCache { provider.defineComponent(hacked(id), files.toSeq.toArray) }
  /** Retrieve the file for component 'id' from the local repository. */
  private def update(id: String): Unit = 
    if(isHacked(id)) {
      // Grab jar magically
      println("Grabbing hacked jar for ["+id+"] = " + hacks.get(id))
      define(hacked(id), Seq(hacks(id)))
    } else ivyCache.withCachedJar(sbtModuleID(id), Some(globalLock), log) {jar => 
      define(id, Seq(jar)) 
    }


  //---------------------!!!!!!!!!!!!!!!!----------------------------------------
  // YUK -  Ugly hack to use newer compiler interface - YUK
  //---------------------!!!!!!!!!!!!!!!!----------------------------------------
  private def sbtModuleID(id: String) = if(isHacked(id)) {
    println("Making hackd module id!")
    val version = "0.12.0-M2" // HACKED 20120314203952
    val timestamp = "20120315T001212" // Hacked  "20120314203952" //
    val stampedVersion = version + "_" + timestamp
    val result = ModuleID("org.scala-sbt", id, stampedVersion)
    println("Looking for hacked module id: " + result)
    result
  } else ModuleID("org.scala-tools.sbt", id, ComponentManager.stampedVersion)
  //---------------------!!!!!!!!!!!!!!!!----------------------------------------
  // ENDYUK -  Ugly hack to use newer compiler interface - ENDYUK
  //---------------------!!!!!!!!!!!!!!!!----------------------------------------

  /** Install the files for component 'id' to the local repository.  This is usually used after writing files to the directory returned by 'location'. */
  override def cache(id: String): Unit = ivyCache.cacheJar(sbtModuleID(id), file(id)(IfMissing.Fail), Some(globalLock), log)
  override def clearCache(id: String): Unit = lockGlobalCache { ivyCache.clearCachedJar(sbtModuleID(id), Some(globalLock), log) }
}

