/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package settings

import scala.tools.util.PathResolver.Defaults

/** Settings which aren't behind a -X, -Y, or -P option.
 *  When possible, the val and the option have identical names.
 *  The abstract settings are commented as to why they are as yet
 *  implemented in MutableSettings rather than mutation-generically.
 */
trait StandardScalaSettings {
  self: AbsScalaSettings =>

  /** Path related settings.
   */
  val bootclasspath =     PathSetting ("-bootclasspath", "path", "Override location of bootstrap class files", Defaults.scalaBootClassPath)
  val classpath:          PathSetting // is mutated directly in various places (thus inspiring this very effort)
  val d:                OutputSetting // depends on mutable OutputDirs class
  val extdirs =           PathSetting ("-extdirs", "dirs", "Override location of installed extensions", Defaults.scalaExtDirs)
  val javabootclasspath = PathSetting ("-javabootclasspath", "path", "Override java boot classpath.", Defaults.javaBootClassPath)
  val javaextdirs =       PathSetting ("-javaextdirs", "path", "Override java extdirs classpath.", Defaults.javaExtDirs)
  val sourcepath =        PathSetting ("-sourcepath", "path", "Specify where to find input source files", "") // Defaults.scalaSourcePath

  /** Other settings.
   */
  val dependencyfile =  StringSetting ("-dependencyfile", "file", "Specify the file in which dependencies are tracked", ".scala_dependencies")
  val deprecation =    BooleanSetting ("-deprecation", "Output source locations where deprecated APIs are used")
  val encoding =        StringSetting ("-encoding", "encoding", "Specify character encoding used by source files", Properties.sourceEncoding)
  val explaintypes =   BooleanSetting ("-explaintypes", "Explain type errors in more detail")
  val g =               ChoiceSetting ("-g", "Specify level of generated debugging info", List("none", "source", "line", "vars", "notailcalls"), "vars")
  val help =           BooleanSetting ("-help", "Print a synopsis of standard options")
  val make =            ChoiceSetting ("-make", "Specify recompilation detection strategy", List("all", "changed", "immediate", "transitive", "transitivenocp"), "all") .
                                          withHelpSyntax("-make:<strategy>")
  val nowarn =         BooleanSetting ("-nowarn", "Generate no warnings")
  val optimise:        BooleanSetting // depends on post hook which mutates other settings
  val print =          BooleanSetting ("-print", "Print program with all Scala-specific features removed")
  val target =          ChoiceSetting ("-target", "Specify for which target object files should be built", List("jvm-1.5", "msil"), "jvm-1.5")
  val unchecked =      BooleanSetting ("-unchecked", "Enable detailed unchecked warnings")
  val uniqid =         BooleanSetting ("-uniqid", "Print identifiers with unique names for debugging")
  val usejavacp =      BooleanSetting ("-usejavacp", "Utilize the java.class.path in classpath resolution.")
  val verbose =        BooleanSetting ("-verbose", "Output messages about what the compiler is doing")
  val version =        BooleanSetting ("-version", "Print product version and exit")

  /** These are @<file> and -Dkey=val style settings, which don't
   *  nicely map to identifiers.
   */
  val argfiles: BooleanSetting  // exists only to echo help message, should be done differently
  val defines: DefinesSetting   // not entirely clear that DefinesSetting makes sense as a Setting
}
