/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
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
  val bootclasspath =     PathSetting ("-bootclasspath", "Override location of bootstrap class files.", Defaults.scalaBootClassPath)
  val classpath:          PathSetting // is mutated directly in various places (thus inspiring this very effort)
  val d:                OutputSetting // depends on mutable OutputDirs class
  val extdirs =           PathSetting ("-extdirs", "Override location of installed extensions.", Defaults.scalaExtDirs)
  val javabootclasspath = PathSetting ("-javabootclasspath", "Override java boot classpath.", Defaults.javaBootClassPath)
  val javaextdirs =       PathSetting ("-javaextdirs", "Override java extdirs classpath.", Defaults.javaExtDirs)
  val sourcepath =        PathSetting ("-sourcepath", "Specify location(s) of source files.", "") // Defaults.scalaSourcePath

  /** Other settings.
   */
  val dependencyfile =  StringSetting ("-dependencyfile", "file", "Set dependency tracking file.", ".scala_dependencies")
  val deprecation =    BooleanSetting ("-deprecation", "Emit warning and location for usages of deprecated APIs.")
  val encoding =        StringSetting ("-encoding", "encoding", "Specify character encoding used by source files.", Properties.sourceEncoding)
  val explaintypes =   BooleanSetting ("-explaintypes", "Explain type errors in more detail.")
  val feature =        BooleanSetting ("-feature", "Emit warning and location for usages of features that should be imported explicitly.")
  val g =               ChoiceSetting ("-g", "level", "Set level of generated debugging info.", List("none", "source", "line", "vars", "notailcalls"), "vars")
  val help =           BooleanSetting ("-help", "Print a synopsis of standard options")
  val make =            ChoiceSetting ("-make", "policy", "Recompilation detection policy", List("all", "changed", "immediate", "transitive", "transitivenocp"), "all")
                        . withDeprecationMessage ("this option is unmaintained.  Use sbt or an IDE for selective recompilation.")
  val nowarn =         BooleanSetting ("-nowarn", "Generate no warnings.")
  val optimise:        BooleanSetting // depends on post hook which mutates other settings
  val print =          BooleanSetting ("-print", "Print program with Scala-specific features removed.")
  val target =          ChoiceSetting ("-target", "target", "Target platform for object files.", List("jvm-1.5", "jvm-1.5-asm", "jvm-1.6", "jvm-1.7", "msil"), "jvm-1.5")
  val unchecked =      BooleanSetting ("-unchecked", "Enable detailed unchecked (erasure) warnings.")
  val uniqid =         BooleanSetting ("-uniqid", "Uniquely tag all identifiers in debugging output.")
  val usejavacp =      BooleanSetting ("-usejavacp", "Utilize the java.class.path in classpath resolution.")
  val verbose =        BooleanSetting ("-verbose", "Output messages about what the compiler is doing.")
  val version =        BooleanSetting ("-version", "Print product version and exit.")

  /** These are @<file> and -Dkey=val style settings, which don't
   *  nicely map to identifiers.
   */
  val argfiles: BooleanSetting  // exists only to echo help message, should be done differently
}
