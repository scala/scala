/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import util.Position
import scala.util.control.Exception.ignoring

/**
 *  Machinery for the asynchronous initialization of the repl.
 */
trait ILoopInit {
  self: ILoop =>

  /** Print a welcome message */
  def printWelcome() {
    import Properties._
    val welcomeMsg =
     """|Welcome to Scala %s (%s, Java %s).
        |Type in expressions to have them evaluated.
        |Type :help for more information.""" .
    stripMargin.format(versionString, javaVmName, javaVersion)
    echo(welcomeMsg)
    replinfo("[info] started at " + new java.util.Date)
  }

  protected def asyncMessage(msg: String) {
    if (isReplInfo || isReplPower)
      echoAndRefresh(msg)
  }

  private val initLock = new java.util.concurrent.locks.ReentrantLock()
  private val initCompilerCondition = initLock.newCondition() // signal the compiler is initialized
  private val initLoopCondition = initLock.newCondition()     // signal the whole repl is initialized
  private val initStart = System.nanoTime

  private def withLock[T](body: => T): T = {
    initLock.lock()
    try body
    finally initLock.unlock()
  }
  // a condition used to ensure serial access to the compiler.
  @volatile private var initIsComplete = false
  private def elapsed() = "%.3f".format((System.nanoTime - initStart).toDouble / 1000000000L)

  // the method to be called when the interpreter is initialized.
  // Very important this method does nothing synchronous (i.e. do
  // not try to use the interpreter) because until it returns, the
  // repl's lazy val `global` is still locked.
  protected def initializedCallback() = withLock(initCompilerCondition.signal())

  // Spins off a thread which awaits a single message once the interpreter
  // has been initialized.
  protected def createAsyncListener() = {
    io.spawn {
      withLock(initCompilerCondition.await())
      asyncMessage("[info] compiler init time: " + elapsed() + " s.")
      postInitialization()
    }
  }

  // called from main repl loop
  protected def awaitInitialized() {
    if (!initIsComplete)
      withLock { while (!initIsComplete) initLoopCondition.await() }
  }
  // private def warningsThunks = List(
  //   () => intp.bind("lastWarnings", "" + typeTag[List[(Position, String)]], intp.lastWarnings _),
  // )

  protected def postInitThunks = List[Option[() => Unit]](
    Some(intp.setContextClassLoader _),
    if (isReplPower) Some(() => enablePowerMode(true)) else None
  ).flatten
  // ++ (
  //   warningsThunks
  // )
  // called once after init condition is signalled
  protected def postInitialization() {
    postInitThunks foreach (f => addThunk(f()))
    runThunks()
    initIsComplete = true

    if (isAsync) {
      asyncMessage("[info] total init time: " + elapsed() + " s.")
      withLock(initLoopCondition.signal())
    }
  }
  // code to be executed only after the interpreter is initialized
  // and the lazy val `global` can be accessed without risk of deadlock.
  private var pendingThunks: List[() => Unit] = Nil
  protected def addThunk(body: => Unit) = synchronized {
    pendingThunks :+= (() => body)
  }
  protected def runThunks(): Unit = synchronized {
    if (pendingThunks.nonEmpty)
      repldbg("Clearing " + pendingThunks.size + " thunks.")

    while (pendingThunks.nonEmpty) {
      val thunk = pendingThunks.head
      pendingThunks = pendingThunks.tail
      thunk()
    }
  }
}
