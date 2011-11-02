/** Scala SBT build
 *  Copyright 2005-2010 LAMP/EPFL
 *  @author  Paul Phillips
 */

import sbt._

/** Worked out a way to fork sbt tasks, preserving all sbt command line
 *  options and without hardcoding anything.
 */
trait ForkSBT {
  self: BasicLayer =>

  def jvmArguments: List[String] = {
    import scala.collection.jcl.Conversions._
    import java.lang.management.ManagementFactory
    ManagementFactory.getRuntimeMXBean().getInputArguments().toList
  }
  
  private var extraJVMArgs: List[String] = Nil
  def withJVMArgs[T](args: String*)(body: => T): T = {
    val saved = extraJVMArgs
    extraJVMArgs = args.toList
    try { body }
    finally extraJVMArgs = saved
  }

  // Set a property in forked sbts to inhibit possible forking cycles.
  def markForked = "-D" + forkProperty + "=true"
  
  /** Forks a new process to run "sbt task task ...":
   */
  def forkTasks(tasks: String*): Boolean = {
    require (!isForked, "Tried to fork but sbt is already forked: " + tasks.mkString(" "))
    
    val sbtJar  = System.getProperty("java.class.path")
    val sbtMain = "xsbt.boot.Boot"  // ok, much of anything.
    val args   = jvmArguments ++ Seq(markForked, "-classpath", sbtJar, sbtMain) ++ tasks
    
    log.info("Forking: " + args.mkString("java ", " ", ""))
    Fork.java(None, args, StdoutOutput) == 0
  }
  def maybeFork(task: TaskManager#Task): Option[String] = maybeFork(task, "Error during external compilation.")
  def maybeFork(task: TaskManager#Task, errorMsg: String): Option[String] = {
    if (isForked) task.run
    else if (forkTasks("project " + this.name, task.name)) None
    else Some(errorMsg)
  }
}
