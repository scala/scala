package scala.reflect.makro
package util

trait Traces {
  def globalSettings: tools.nsc.Settings

  // [Eugene] lots of ways to log:
  // 1) trace(...)
  // 2) log(...)
  // 3) if (foo) { doStuff(); includingSomeLogs(); }
  // what is the conventional way of unifying this?
  val macroDebugLite = globalSettings.YmacrodebugLite.value
  val macroDebugVerbose = globalSettings.YmacrodebugVerbose.value
  val macroTraceLite = scala.tools.nsc.util.trace when (macroDebugLite || macroDebugVerbose)
  val macroTraceVerbose = scala.tools.nsc.util.trace when macroDebugVerbose
  @inline final def macroLogLite(msg: => Any) { if (macroDebugLite || macroDebugVerbose) println(msg) }
  @inline final def macroLogVerbose(msg: => Any) { if (macroDebugVerbose) println(msg) }
}