package scala.tools.nsc.classpath

import java.util.concurrent.atomic.AtomicReference
import java.util.{List => JList}
import java.nio.file._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.ref.WeakReference


object ClassPathWatcher {

  private val watchServices = mutable.Map[FileSystem, WatchInfo]()

  private val CHANGES = Array[WatchEvent.Kind[_]](
    StandardWatchEventKinds.ENTRY_CREATE,
    StandardWatchEventKinds.ENTRY_DELETE,
    StandardWatchEventKinds.ENTRY_MODIFY
  )

  private class WatchInfo(fileSystem: FileSystem) extends Runnable{
    val watcher = fileSystem.newWatchService()
    private val runningThread = new AtomicReference[Thread]

    val watched = mutable.Map.empty[WatchKey, WeakReference[BaseChangeListener]]

    def addWatch(listener: BaseChangeListener, path:Path): WatchKey = {
      watched.synchronized {
        val key = path.register(watcher, CHANGES)
        watched.put(key, WeakReference(listener))
        key
      }
    }
    def removeWatch(listener: BaseChangeListener, keys: WatchKey *): Unit =  watched.synchronized  {
      keys foreach cancel
    }

    def run(): Unit = {
      import collection.JavaConverters._
      while (true) {
        val changed = watcher.take()
        val events = changed.pollEvents().asInstanceOf[JList[WatchEvent[Path]]]
        trace(s"detected a change ${events} - ${changed.watchable} ")
        if (!changed.isValid) {
          trace(s"Key invalid")
          cancel(changed)
        } else watched.synchronized(watched.get(changed)) match {
          case Some(WeakReference(callback)) =>
            callback.changed(changed, events.asScala)
            if (!changed.isValid) {
              trace(s"key invalid after callback. key cancelled")
              cancel(changed)
            } else changed.reset()
          case None =>
            trace(s"No associated watcher. key cancelled")
            cancel(changed)
        }
      }
    }
    private def cancel(key:WatchKey) = {
      key.cancel()
      watched.synchronized( watched.remove(key))
    }
  }

  private def getWatchInfo(path: Path) : WatchInfo = {
    val fs = path.getFileSystem
    watchServices.synchronized {watchServices.getOrElseUpdate(fs, new WatchInfo(fs))}
  }

  //hook to allow toggling of trace driven by settings, so public
  var tracing = false
  @inline private[ClassPathWatcher] def trace(msg: => String) {
    if (tracing) println(s"Watcher - $msg")
  }


  abstract class BaseChangeListener (path:Path) {

    def resumeIfSuspended(): Unit = if (suspended) resume(true)

    private [ClassPathWatcher] def changed(key:WatchKey, events:Seq[WatchEvent[Path]])
    @volatile private[ClassPathWatcher] var suspended = false

    @tailrec private [ClassPathWatcher] final def clearPendingChanges(key:WatchKey): Unit = {
      if (!key.pollEvents().isEmpty) clearPendingChanges(key)
    }
    protected def resume(clearPending:Boolean): Unit
  }
  abstract class FileChangeListener(path: Path) extends BaseChangeListener(path) {
    require (Files.isRegularFile(path))

    private [ClassPathWatcher]val watchInfo = getWatchInfo(path.getParent)
    private [ClassPathWatcher]val key = watchInfo.addWatch(this, path.getParent)

    protected override final def finalize(): Unit = {
      watchInfo.removeWatch(this, key)
    }

    override final def changed(key: WatchKey, events:Seq[WatchEvent[Path]]): Unit = {
      import collection.JavaConverters._
      require(key eq this.key)

      val events: List[WatchEvent[Path]] = key.pollEvents().asScala.collect {
        case e: WatchEvent[Path] @unchecked if path.endsWith(e.context()) => e
      }(collection.breakOut)

      if (events.isEmpty) {
        trace(s"all events are filtered, reset")
        key.reset()
      } else if (suspended) {
        //dont need to record time as it will be reset for resume
        trace(s"change ignored - suspended, $events")
      } else {
        trace(s"change detected, $events")
        if (fileChanged(events)) {
          trace(s"resetting")
          key.reset()
        } else {
          trace(s"suspending")
          suspended = true
        }
      }
    }

    /**
      * inform the listener that the file has changed
      * @param events the changes
      * @return true to continue monitoring, false to suspend monitoring until the next call to {{{resume()}}}
      */
    protected def fileChanged(events:List[WatchEvent[Path]]) : Boolean

    final def resume(clearPending:Boolean): Unit = {
      suspended = false
      if (clearPending) clearPendingChanges(key)
      key.reset()
    }
  }

  abstract class DirectoryChangeListener(path: Path) extends BaseChangeListener(path) {
    require (Files.isDirectory(path))

    private [ClassPathWatcher]val watchInfo = getWatchInfo(path.getParent)
    private [ClassPathWatcher]val keysToPath = mutable.Map.empty[WatchKey, Path]

    protected override final def finalize(): Unit = {
      watchInfo.removeWatch(this, keysToPath.keySet.toSeq : _*)
    }

    def addDirWatch(path:Path): Unit = keysToPath.synchronized{
      val key = watchInfo.addWatch(this, path)
      keysToPath(key) = path
    }
    def removeAllWatches(): Unit = keysToPath.synchronized{
      watchInfo.removeWatch(this, keysToPath.keySet.toSeq : _*)
      keysToPath.clear()
    }

    override final def changed(key: WatchKey, events:Seq[WatchEvent[Path]]): Unit = keysToPath.synchronized{
      import collection.JavaConverters._
      val path = keysToPath(key)
      val events:List[WatchEvent[Path]] = key.pollEvents().asScala.map{case e: WatchEvent[Path] @unchecked => e}(collection.breakOut)

      if (suspended) {
        //dont need to record time as it will be reset for resume
        trace(s"change ignored - suspended, $events")
      } else {
        trace(s"change detected, $events")
        if (dirChanged(path, events)) {
          trace(s"resetting")
          key.reset()
        } else {
          trace(s"suspending")
          suspended = true
        }
      }
    }

    /**
      * inform the listener that chnages have occured to a directory
      * @param path the directory that changed
      * @param events the changes
      * @return true to continue monitoring, false to suspend monitoring until the next call to {{{resume()}}}
      */
    protected def dirChanged(path:Path, events:List[WatchEvent[Path]]) : Boolean

    final def resume(clearPending:Boolean): Unit = keysToPath.synchronized{
      suspended = false
      if (clearPending) keysToPath.keys foreach clearPendingChanges

      keysToPath.keys.foreach {_.reset()}
    }
  }
}
