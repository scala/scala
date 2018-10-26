/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
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
  val bootclasspath =     PathSetting ("-bootclasspath", "Override location of bootstrap class files.", Defaults.scalaBootClassPath) withAbbreviation "--boot-class-path"
  val classpath:          PathSetting // is mutated directly in various places (thus inspiring this very effort)
  val d:                OutputSetting // depends on mutable OutputDirs class
  val extdirs =           PathSetting ("-extdirs", "Override location of installed extensions.", Defaults.scalaExtDirs) withAbbreviation "--extension-directories"
  val javabootclasspath = PathSetting ("-javabootclasspath", "Override java boot classpath.", Defaults.javaBootClassPath) withAbbreviation "--java-boot-class-path"
  val javaextdirs =       PathSetting ("-javaextdirs", "Override java extdirs classpath.", Defaults.javaExtDirs) withAbbreviation "--java-extension-directories"
  val sourcepath =        PathSetting ("-sourcepath", "Specify location(s) of source files.", "") withAbbreviation "--source-path" // Defaults.scalaSourcePath

  /** Other settings.
   */
  val dependencyfile =  StringSetting ("-dependencyfile", "file", "Set dependency tracking file.", ".scala_dependencies") withAbbreviation "--dependency-file"
  val deprecation =    BooleanSetting ("-deprecation", "Emit warning and location for usages of deprecated APIs.") withAbbreviation "--deprecation"
  val encoding =        StringSetting ("-encoding", "encoding", "Specify character encoding used by source files.", Properties.sourceEncoding) withAbbreviation "--encoding"
  val explaintypes =   BooleanSetting ("-explaintypes", "Explain type errors in more detail.") withAbbreviation "--explain-types"
  val feature =        BooleanSetting ("-feature", "Emit warning and location for usages of features that should be imported explicitly.") withAbbreviation "--feature"
  val g =               ChoiceSetting ("-g", "level", "Set level of generated debugging info.", List("none", "source", "line", "vars", "notailcalls"), "vars")
  val help =           BooleanSetting ("-help", "Print a synopsis of standard options") withAbbreviation "--help"
  val nowarn =         BooleanSetting ("-nowarn", "Generate no warnings.") withAbbreviation "--no-warnings"
  val optimise:        BooleanSetting // depends on post hook which mutates other settings
  val print =          BooleanSetting ("-print", "Print program with Scala-specific features removed.") withAbbreviation "--print"
  val target =         ChoiceSettingForcedDefault ("-target", "target", "Target platform for object files. All JVM 1.5 - 1.7 targets are deprecated.",
                          List("jvm-1.5", "jvm-1.6", "jvm-1.7", "jvm-1.8"), "jvm-1.8") withAbbreviation "--target"
  val unchecked =      BooleanSetting ("-unchecked", "Enable additional warnings where generated code depends on assumptions.") withAbbreviation "--unchecked"
  val uniqid =         BooleanSetting ("-uniqid", "Uniquely tag all identifiers in debugging output.") withAbbreviation "--unique-id"
  val usejavacp =      BooleanSetting ("-usejavacp", "Utilize the java.class.path in classpath resolution.") withAbbreviation "--use-java-class-path"
  val usemanifestcp =  BooleanSetting ("-usemanifestcp", "Utilize the manifest in classpath resolution.") withAbbreviation "--use-manifest-class-path"
  val verbose =        BooleanSetting ("-verbose", "Output messages about what the compiler is doing.") withAbbreviation "--verbose"
  val version =        BooleanSetting ("-version", "Print product version and exit.") withAbbreviation "--version"
}
