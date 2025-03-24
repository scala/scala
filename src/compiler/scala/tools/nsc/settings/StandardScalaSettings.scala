/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
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
import scala.util.Properties.{isJavaAtLeast, javaSpecVersion}

/** Settings which aren't behind a -V, -W, -X, -Y, or -P option.
 *  When possible, the val and the option have identical names.
 */
trait StandardScalaSettings { _: MutableSettings =>

  import StandardScalaSettings._

  /** Path related settings.
   */
  val bootclasspath =     PathSetting ("-bootclasspath", "Override location of bootstrap class files.", Defaults.scalaBootClassPath) withAbbreviation "--boot-class-path"
  val classpath:          PathSetting // is mutated directly in various places (thus inspiring this very effort)
  val extdirs =           PathSetting ("-extdirs", "Override location of installed extensions.", Defaults.scalaExtDirs) withAbbreviation "--extension-directories"
  val javabootclasspath = PathSetting ("-javabootclasspath", "Override java boot classpath.", Defaults.javaBootClassPath) withAbbreviation "--java-boot-class-path"
  val javaextdirs =       PathSetting ("-javaextdirs", "Override java extdirs classpath.", Defaults.javaExtDirs) withAbbreviation "--java-extension-directories"
  val sourcepath =        PathSetting ("-sourcepath", "Specify location(s) of source files.", "") withAbbreviation "--source-path" // Defaults.scalaSourcePath
  val rootdir =           PathSetting ("-rootdir", "The absolute path of the project root directory, usually the git/scm checkout. Used by -Wconf.", "") withAbbreviation "--root-directory"
  val systemPath =        PathSetting ("-system", "Override location of Java system modules", "") withAbbreviation "--system"

  /** Other settings.
   */
  val dependencyfile =  StringSetting ("-dependencyfile", "file", "Set dependency tracking file.", ".scala_dependencies") withAbbreviation "--dependency-file"
  val deprecation =    BooleanSetting ("-deprecation", "Emit warning and location for usages of deprecated APIs. See also -Wconf.").withAbbreviation("--deprecation")
  val encoding =        StringSetting ("-encoding", "encoding", "Specify character encoding used by source files.", Properties.sourceEncoding) withAbbreviation "--encoding"
  val explaintypes =   BooleanSetting ("-explaintypes", "Explain type errors in more detail.") withAbbreviation "--explain-types"
  val feature =        BooleanSetting ("-feature", "Emit warning and location for usages of features that should be imported explicitly. See also -Wconf.") withAbbreviation "--feature" withPostSetHook { s =>
    if (s.value) Wconf.tryToSet(List(s"cat=feature:w"))
    else Wconf.tryToSet(List(s"cat=feature:s"))
  }
  val g =               ChoiceSetting ("-g", "level", "Set level of generated debugging info.", List("none", "source", "line", "vars", "notailcalls"), "vars")
  val help =           BooleanSetting ("-help", "Print a synopsis of standard options") withAbbreviation "--help" withAbbreviation("-h")
  val nowarn =         BooleanSetting("-nowarn", "Silence warnings. (-Wconf:any:s)")
                        .withAbbreviation("--no-warnings")
                        .withPostSetHook(s => if (s.value) maxwarns.value = 0)
  val optimise:        BooleanSetting // depends on post hook which mutates other settings
  val print =          BooleanSetting ("-print", "Print program with Scala-specific features removed.") withAbbreviation "--print"
  val quickfix =       MultiStringSetting(
    "-quickfix",
    "filters",
    "Apply quick fixes provided by the compiler for warnings and errors to source files",
    helpText = Some(
      """Apply quick fixes provided by the compiler for warnings and errors to source files.
        |Syntax: -quickfix:<filter>,...,<filter>
        |
        |<filter> syntax is the same as for configurable warnings, see `-Wconf:help`. Examples:
        |  -quickfix:any                    apply all available quick fixes
        |  -quickfix:msg=Auto-application   apply quick fixes where the message contains "Auto-application"
        |
        |Use `-Wconf:any:warning-verbose` to display applicable message filters with each warning.
        |
        |Use `-quickfix:silent` to omit the `[quickfixable]` tag in compiler messages.
        |""".stripMargin),
    prepend = true)
  def quickFixSilent: Boolean = quickfix.value == List("silent")
  val release =
    ChoiceSetting("-release", "release", "Compile for a version of the Java API and target class file.", AllTargetVersions, normalizeTarget(javaSpecVersion))
      .withPostSetHook { setting =>
        val current = setting.value.toInt
        if (!isJavaAtLeast("9") && current > 8) errorFn.apply("-release is only supported on JVM 9 and higher")
        if (target.valueSetByUser.map(_.toInt > current).getOrElse(false)) errorFn("-release cannot be less than -target")
        if (systemPath.isSetByUser) errorFn("-release cannot be used with -system")
        //target.value = setting.value  // this would trigger deprecation
      }
      .withAbbreviation("--release")
      .withAbbreviation("-java-output-version")
  def releaseValue: Option[String] = release.valueSetByUser
  def systemPathValue: Option[String] = systemPath.valueSetByUser
  val target =
    ChoiceSetting("-target", "target", "Target platform for object files.", AllTargetVersions, "8")
      .withPreSetHook(normalizeTarget)
      .withPostSetHook { setting =>
        if (releaseValue.map(_.toInt < setting.value.toInt).getOrElse(false)) errorFn("-release cannot be less than -target")
      }
      .withAbbreviation("--target")
      // .withAbbreviation("--Xtarget")
      // .withAbbreviation("-Xtarget")
      .withAbbreviation("-Xunchecked-java-output-version")
  def targetValue: String = target.valueSetByUser.orElse(releaseValue).getOrElse(target.value)
  val unchecked =      BooleanSetting ("-unchecked", "Enable additional warnings where generated code depends on assumptions. See also -Wconf.") withAbbreviation "--unchecked" withPostSetHook { s =>
    if (s.value) Wconf.tryToSet(List(s"cat=unchecked:w"))
    else Wconf.tryToSet(List(s"cat=unchecked:s"))
  }
  val uniqid =         BooleanSetting ("-uniqid", "Uniquely tag all identifiers in debugging output.") withAbbreviation "--unique-id"
  val usejavacp =      BooleanSetting ("-usejavacp", "Utilize the java.class.path in classpath resolution.") withAbbreviation "--use-java-class-path"
  val usemanifestcp =  BooleanSetting ("-usemanifestcp", "Utilize the manifest in classpath resolution.") withAbbreviation "--use-manifest-class-path"
  val verbose =        BooleanSetting ("-verbose", "Output messages about what the compiler is doing.") withAbbreviation "--verbose"
  val version =        BooleanSetting ("-version", "Print product version and exit.") withAbbreviation "--version"

  // Support passe prefixes of -target values:
  //   - `jvm-` (from back when we also had `msil`)
  //   - `1.` (from back when Java 2 was a possibility)
  // Otherwise, `-release` could be `IntSetting`.
  private def normalizeTarget(in: String): String = {
    val jvmish = raw"jvm-(\d*)".r
    in match {
      case "1.8" | "jvm-1.8" => "8"
      case jvmish(n) => n
      case n => n
    }
  }
}

object StandardScalaSettings {
  val MinTargetVersion = 8
  val MaxTargetVersion = ScalaVersion(javaSpecVersion) match {
    case SpecificScalaVersion(1, minor, _, _) => minor
    case SpecificScalaVersion(major, _, _, _) => major
    case _ => 24
  }

  private val AllTargetVersions = (MinTargetVersion to MaxTargetVersion).map(_.toString).to(List)
}
