package scala.tools.nsc.util

import java.net.URL
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.tools.nsc.classpath._
import scala.tools.nsc.io.AbstractFile
import scala.collection.{mutable, Set => SSet}


/**
  * factory for classpath where some result are cached
  */
object CachedClassPath {
  def apply(classPath:ClassPath): CachedClassPath = {
    classPath match {
      case cachedAlready: CachedClassPath => cachedAlready
      case noSources: ClassPath with NoSourcePaths => CachedNoSourcesClassPath(noSources)
      case noClasses: ClassPath with NoClassPaths => CachedNoClassesClassPath(noClasses)
      case other: ClassPath => CachedAnyClassPath(other)
    }
  }
  private case class CachedNoSourcesClassPath(underlying:ClassPath with NoSourcePaths) extends
    ClassesCachedClassPath with NoSourcePaths

  private case class CachedNoClassesClassPath(underlying:ClassPath with NoClassPaths) extends
    SourcesCachedClassPath with NoClassPaths

  private case class CachedAnyClassPath(underlying:ClassPath) extends
    ClassesCachedClassPath with SourcesCachedClassPath

  sealed trait CachedClassPath extends ClassPath {
    protected val underlying: ClassPath

    override def hashCode(): Int = underlying.hashCode() * 31

    override def equals(obj: scala.Any): Boolean = obj match {
      case cached: CachedClassPath => (cached eq this) || (cached.getClass == getClass && (cached.underlying eq underlying))
      case _ => false
    }
    override final lazy val asClassPathStrings: Seq[String] = underlying.asClassPathStrings
    override final lazy val asURLs: Seq[URL] = underlying.asURLs

    private val packagesCache = new CachedLazyMapping(underlying.packages, Nil)

    override private[nsc] def hasPackage(pkg: String) = packagesCache(pkg).isEmpty

    override private[nsc] final def packages(inPackage: String) = packagesCache(inPackage)

    private val listCache = new CachedLazyMapping(underlying.list, ClassPathEntries.empty)
    override private[nsc] final def list(inPackage: String) = listCache(inPackage)

    // preetching status. Cant just roll this into an AtomicInteger though
    // -2 => prefetch not requested
    // -1 => cache valid but incomplete
    // 0 => cache complete
    // +ve number of prefetch requests pending
    // critical atomic actions are synchronized on packagesCache
    private var prefetchStatus = -2

    def startPrefetch(exec: ExecutionContext): Option[Future[Unit]] = packagesCache.synchronized {
      if (prefetchStatus < 0) {
        prefetchStatus = 1
        Some(Future(revalidateNow)(exec))
      }
      else None
    }

    @tailrec private def revalidateNow(): Unit = {
      val initialState = packagesCache.synchronized(prefetchStatus)
      println(s"start prefetch $this - prefetchStatus = $initialState")
      val all = mutable.Set[String]()

      def load(names: Iterable[PackageEntry]): Unit = {
        names foreach { p =>
          all += p.name
          load(packages(p.name))
        }
      }

      load(packages(""))
      val cachedMappins = mappings
      cachedMappins foreach { mapping =>
        all.foreach(mapping.apply)
      }
      val retry = packagesCache.synchronized {
        if (prefetchStatus == initialState) {
          cachedMappins foreach { mapping =>
            mapping.markComplete()
            mapping.optimise()
          }
          false
        } else true
      }
      if (retry) revalidateNow()
    }

    def mappings: List[BaseCachedLazyMapping[_]] = packagesCache :: listCache :: Nil

    protected def stats = s"stats packages${packagesCache.stats} list${listCache.stats}"

    override def toString: String = s"${getClass.getSimpleName} $underlying $stats"

    override def startInUse(executionContext: ExecutionContext, proactive:Boolean): Unit = underlying.startInUse(executionContext, proactive)

    override def endInUse(): Unit = underlying.endInUse()

    var lastCacheValidTime = 0L

    override def makeCacheValid(executionContext: ExecutionContext, proactive:Boolean) = {
      val valid = underlying.makeCacheValid(executionContext, proactive)
      if (valid > lastCacheValidTime) listCache.synchronized {
        val cachedMappings = mappings
        // clear the cache
        cachedMappings foreach { _.reset()}
        prefetchStatus match {
          case -1 | -2 if !proactive =>
            //do nothing
          case -1 | -2 =>
            prefetchStatus = 1
            startPrefetch(executionContext)
          case 0 =>
            if (proactive) {
              prefetchStatus = 1
              startPrefetch(executionContext)
            } else {
              prefetchStatus = -2
            }
          case _ =>
            //someone is working on it, but it may be updated, so cause a re-cycle
            prefetchStatus += 1
        }
        lastCacheValidTime = valid
      }
      valid
    }
  }
  private sealed trait ClassesCachedClassPath extends CachedClassPath {

    private val classesCache = new CachedLazyMapping(underlying.classes, Nil)
    override private[nsc] final def classes(inPackage: String) = classesCache(inPackage)

    override def findClassFile(className: String): Option[AbstractFile] = underlying.findClassFile(className)

    override def mappings: List[BaseCachedLazyMapping[_]] = classesCache :: super.mappings

    override protected def stats = s"${super.stats} classes${classesCache.stats}"
  }
  private sealed trait SourcesCachedClassPath  extends CachedClassPath{

    private val sourcesCache = new CachedLazyMapping(underlying.sources, Nil)
    override private[nsc] def sources(inPackage: String) = sourcesCache(inPackage)

    override lazy val asSourcePathString: String = underlying.asSourcePathString

    override def mappings: List[BaseCachedLazyMapping[_]] = sourcesCache :: super.mappings

    override protected def stats = s"${super.stats} sources${sourcesCache.stats}"
  }

}

/**
  * a Cached implementation of a mapping. This has to be threadsafe as a classpath may be cached and reused in a JVM
  * @tparam V
  */
abstract class BaseCachedLazyMapping[V <: AnyRef] {

  @volatile protected var complete = false
  protected val cache: ConcurrentHashMap[String,_]

  protected val hits = new AtomicInteger
  protected val misses = new AtomicInteger
  protected val defaulted = new AtomicInteger

  def apply(key:String): V
  def markComplete(): Unit = {
    this.complete = true
  }
  def optimise()
  def stats = s"H$hits M$misses D$defaulted"
  def reset() :Unit = {
    cache.clear()
    complete = false
  }
}
class CachedLazyMapping[V <: AnyRef] (miss : (String => V), defaultValue: V ) extends BaseCachedLazyMapping[V] {
  override protected val cache =  new ConcurrentHashMap[String,V]

  def apply(key:String): V = {
    //note - order dependent. Must be before we check the cache
    val alreadyComplete = complete
    val cached = cache.get(key)
    if (cached ne null) {
      hits.incrementAndGet()
      cached
    } else if (alreadyComplete) {
      defaulted.incrementAndGet()
      defaultValue
    } else {
      misses.incrementAndGet()
      val missed = miss(key)
      val existing = cache.putIfAbsent(key, missed)
      if (existing ne null) existing else missed
    }
  }
  override def optimise : Unit = {
    //remove the default values
    //cant use cache.values.remove(All), they only remove a single instance
    val it = cache.entrySet().iterator()
    while (it.hasNext()) {
      val curr = it.next()
      if (curr.getValue == defaultValue) it.remove()
    }
  }
}