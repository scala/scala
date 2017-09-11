package scala.tools.nsc.classpath

import java.nio.file.{Files, Path, WatchEvent}
import java.nio.file.attribute.BasicFileAttributes
import scala.tools.nsc.classpath.ClassPathWatcher._



trait LivenessChecker {

  def startInUse(proactive:Boolean) :Unit

  def endInUse() :Unit

  def validateAndLastModificationTime():Long

  def removeAllWatches(): Unit = ()

  def addDirWatch(dir:Path): Unit = ()

}
object LivenessChecker {
  sealed trait ForDir {
    def apply(classPath : CommonDirClassPath[_]) : LivenessChecker
  }
  sealed trait ForFile {
    def apply(classPath : CommonZipClassPath[_]) : LivenessChecker
  }
  /**
    * Policy to assume that the resource cannot change - useful for jar in maven repos and simple one off compiles
   */
  case object Static extends ForDir with ForFile{

    override def apply(classPath: CommonZipClassPath[_]): LivenessChecker = init(classPath)

    override def apply(classPath: CommonDirClassPath[_]): LivenessChecker = init(classPath)
    private def init(classPath: CommonClassPath[_]) = {
      classPath.ensureContent(true)
      StaticLivenessChecker
    }

    private object StaticLivenessChecker extends LivenessChecker{
      override def startInUse(proactive: Boolean): Unit = ()

      override def endInUse(): Unit = ()

      override def validateAndLastModificationTime(): Long = 0L
    }
  }
  /**
    * Policy to assume that the resource has changed when examined, if not already in use
    * use where watcher doesnt work
    */
  case object NotMonitored extends ForDir with ForFile{
    override def apply(classPath: CommonZipClassPath[_]): LivenessChecker =
      new NotMonitoredLivenessChecker(classPath)

    override def apply(classPath: CommonDirClassPath[_]): LivenessChecker =
      new NotMonitoredLivenessChecker(classPath)

    private class NotMonitoredLivenessChecker(classPath: CommonClassPath[_]) extends UsageTrackedLivenessChecker(classPath){
      var lastCacheTime = System.nanoTime()
      override def doStartInUse(proactive: Boolean): Unit = {
        classPath.ensureContent(proactive)
      }

      override def doEndInUse(): Unit = {
        classPath.markInvalid()
        lastCacheTime = System.nanoTime()
      }

      override def validateAndLastModificationTime(): Long = lastCacheTime
    }
  }

  case object SizeAndDateWatcher extends ForFile {
    override def apply(classPath: CommonZipClassPath[_]):LivenessChecker = new SizeAndDateLivenessChecker(classPath)

    private class SizeAndDateLivenessChecker(classPath: CommonZipClassPath[_]) extends UsageTrackedLivenessChecker(classPath){
      val path = classPath.rootFile.toPath

      private var lastCacheTime = 0L

      private var fileInfo = Option.empty[BasicFileAttributes]

      /**
        * although [[doStartInUse]] is guaranteed to externally only be called once, the implementation is safe
        * as masked with BasicAttributtes check, so we call that to encorage early opening of the file in background
        */
      doStartInUse(true)

      override def doStartInUse(proactive: Boolean): Unit = {
        val newAttributes = Files.readAttributes(path, classOf[BasicFileAttributes])
        val stillValid = fileInfo exists {
          oldAttributes => oldAttributes.size() == newAttributes.size() && oldAttributes.lastModifiedTime() == newAttributes.lastModifiedTime()
        }
        if (!stillValid) {
          lastCacheTime = System.nanoTime()
          fileInfo = Some(newAttributes)
          classPath.markInvalid()
          classPath.ensureContent(proactive)
        }
      }

      override def validateAndLastModificationTime(): Long = lastCacheTime
    }
  }
  case object FileWatcher extends ForDir with ForFile {
    override def apply(classPath: CommonDirClassPath[_]):LivenessChecker = new DirWatcherFileLivenessChecker(classPath)

    override def apply(classPath: CommonZipClassPath[_]):LivenessChecker = new FileWatcherFileLivenessChecker(classPath)

    private abstract class FileWatcherLivenessChecker(classPath: CommonClassPath[_]) extends UsageTrackedLivenessChecker(classPath) {

      protected var lastCacheTime = 0L

      /**
        * although [[doStartInUse]] is guaranteed to externally only be called once, the implementation is safe
        * as masked with BasicAttributtes check, so we call that to encorage early opening of the file in background
        */
      override final def doStartInUse(proactive: Boolean): Unit = {
        if (classPath.contents.isEmpty) {
          fileChangeListener.resumeIfSuspended()
          lastCacheTime = System.nanoTime()
          classPath.ensureContent(proactive)
        }
      }

      override def validateAndLastModificationTime(): Long = lastCacheTime

      protected val fileChangeListener : BaseChangeListener
    }
    private class FileWatcherFileLivenessChecker(override val classPath: CommonZipClassPath[_]) extends FileWatcherLivenessChecker(classPath) {

      protected override object fileChangeListener extends FileChangeListener(classPath.rootFile.toPath) {
        override protected def fileChanged(events: List[WatchEvent[Path]]) =
          FileWatcherFileLivenessChecker.this.synchronized {
            classPath.markInvalid()
            false
          }
      }
    }
    //similar to FileWatcherFileLivenessChecker, but has potential oppertunities to detect which directories have changed
    //with changes to the classpath
    private class DirWatcherFileLivenessChecker(override val classPath: CommonDirClassPath[_]) extends FileWatcherLivenessChecker(classPath) {

      protected override object fileChangeListener extends DirectoryChangeListener(classPath.rootFile.toPath) {
        override protected def dirChanged(path: Path, events: List[WatchEvent[Path]]) =
          DirWatcherFileLivenessChecker.this.synchronized {
            classPath.markInvalid()
            false
          }
      }

      override def removeAllWatches(): Unit = {
        fileChangeListener.removeAllWatches()
      }
      override def addDirWatch(dir:Path): Unit = {
        fileChangeListener.addDirWatch(dir)
      }

    }
  }

  abstract class UsageTrackedLivenessChecker(protected val classPath:CommonClassPath[_]) extends LivenessChecker {

    protected final var inUseCount = 0
    protected final val lock = new Object

    protected def doStartInUse(proactive: Boolean): Unit
    protected def doEndInUse(): Unit = ()

    final override def startInUse(proactive: Boolean): Unit = lock.synchronized {
      inUseCount += 1
      if (inUseCount == 1)
        doStartInUse(proactive)
    }

    final override def endInUse(): Unit = lock.synchronized {
      inUseCount -= 1
      if (inUseCount == 0) {
        classPath.contents.foreach (_.close())

        doEndInUse()
      }
    }
  }
}
