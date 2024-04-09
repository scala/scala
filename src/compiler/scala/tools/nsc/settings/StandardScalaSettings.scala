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

import scala.tools.nsc.settings.StandardScalaSettings._
import scala.tools.util.PathResolver.Defaults
import scala.util.Properties.{isJavaAtLeast, javaSpecVersion}

/** Settings which aren't behind a -X, -Y, or -P option.
 *  When possible, the val and the option have identical names.
 *  The abstract settings are commented as to why they are as yet
 *  implemented in MutableSettings rather than mutation-generically.
 */
trait StandardScalaSettings { _: MutableSettings =>
  // Switched to MutableSettings so:
  // 1. deprecation/feature/etc can access Wconf
  // 2. and they have withPostSetHook methods

  /** Path related settings.
   */
  val bootclasspath =     PathSetting ("-bootclasspath", "Override location of bootstrap class files.", Defaults.scalaBootClassPath)
  val classpath:          PathSetting // is mutated directly in various places (thus inspiring this very effort)
  val d:                OutputSetting // depends on mutable OutputDirs class
  val extdirs =           PathSetting ("-extdirs", "Override location of installed extensions.", Defaults.scalaExtDirs)
  val javabootclasspath = PathSetting ("-javabootclasspath", "Override java boot classpath.", Defaults.javaBootClassPath)
  val javaextdirs =       PathSetting ("-javaextdirs", "Override java extdirs classpath.", Defaults.javaExtDirs)
  val sourcepath =        PathSetting ("-sourcepath", "Specify location(s) of source files.", "") // Defaults.scalaSourcePath
  val rootdir =           PathSetting ("-rootdir", "The absolute path of the project root directory, usually the git/scm checkout. Used by -Wconf.", "") withAbbreviation "--root-directory"

  /** Other settings.
   */
  val dependencyfile =  StringSetting ("-dependencyfile", "file", "Set dependency tracking file.", ".scala_dependencies")
  val deprecation =    BooleanSetting ("-deprecation", "Emit warning and location for usages of deprecated APIs. See also -Wconf.") withAbbreviation "--deprecation" withPostSetHook { s =>
    if (s.value) Wconf.tryToSet(List(s"cat=deprecation:w"))
    else Wconf.tryToSet(List(s"cat=deprecation:s"))
  }
  val encoding =        StringSetting ("-encoding", "encoding", "Specify character encoding used by source files.", Properties.sourceEncoding)
  val explaintypes =   BooleanSetting ("-explaintypes", "Explain type errors in more detail.")
  val feature =        BooleanSetting ("-feature", "Emit warning and location for usages of features that should be imported explicitly. See also -Wconf.") withAbbreviation "--feature" withPostSetHook { s =>
    if (s.value) Wconf.tryToSet(List(s"cat=feature:w"))
    else Wconf.tryToSet(List(s"cat=feature:s"))
  }
  val g =               ChoiceSetting ("-g", "level", "Set level of generated debugging info.", List("none", "source", "line", "vars", "notailcalls"), "vars")
  val help =           BooleanSetting ("-help", "Print a synopsis of standard options")
  val nowarn =         BooleanSetting ("-nowarn", "Generate no warnings.") withPostSetHook { s => if (s) maxwarns.value = 0 }
  val optimise:        BooleanSetting // depends on post hook which mutates other settings
  val print =          BooleanSetting ("-print", "Print program with Scala-specific features removed.")
  val release =
    ChoiceSetting("-release", "release", "Compile for a version of the Java API and target class file.", AllTargetVersions, normalizeTarget(javaSpecVersion))
    .withPostSetHook { setting =>
      val current = setting.value.toInt
      if (!isJavaAtLeast("9") && current > 8) errorFn.apply("-release is only supported on JVM 9 and higher")
      if (target.valueSetByUser.map(_.toInt > current).getOrElse(false)) errorFn("-release cannot be less than -target")
    }
    .withAbbreviation("--release")
    .withAbbreviation("-java-output-version")
  def releaseValue: Option[String] = release.valueSetByUser
  val target =
    ChoiceSetting("-target", "target", "Target platform for class files. Target < 8 is deprecated; target > 8 uses 8.",
      AllTargetVersions, DefaultTargetVersion, AllTargetVersions.map(v => if (v.toInt <= 8) s"uses $v" else "unsupported, uses default 8"))
    .withPreSetHook(normalizeTarget)
    .withPostSetHook { setting =>
      if (releaseValue.map(_.toInt < setting.value.toInt).getOrElse(false))
        errorFn("-release cannot be less than -target")
      if (!setting.deprecationMessage.isDefined)
        if (setting.value.toInt > MaxSupportedTargetVersion) {
          setting.withDeprecationMessage(s"Scala 2.12 cannot emit valid class files for targets newer than $MaxSupportedTargetVersion; this is possible with Scala 2.13. Use -release to compile against a specific version of the platform API.")
          setting.value = DefaultTargetVersion
        } else if (setting.value.toInt < MinSupportedTargetVersion) {
          setting.withDeprecationMessage(s"${setting.name}:${setting.value} is deprecated, forcing use of $DefaultTargetVersion")
          setting.value = DefaultTargetVersion
        }
    }
    .withAbbreviation("--target")
  // Unlike 2.13, don't use `releaseValue.getOrElse(target.value)`, because 2.12 doesn't have a fix for scala-dev#408
  def targetValue: String = target.value
  val unchecked =      BooleanSetting ("-unchecked", "Enable additional warnings where generated code depends on assumptions. See also -Wconf.") withAbbreviation "--unchecked" withPostSetHook { s =>
    if (s.value) Wconf.tryToSet(List(s"cat=unchecked:w"))
    else Wconf.tryToSet(List(s"cat=unchecked:s"))
  }
  val uniqid =         BooleanSetting ("-uniqid", "Uniquely tag all identifiers in debugging output.")
  val usejavacp =      BooleanSetting ("-usejavacp", "Utilize the java.class.path in classpath resolution.")
  val usemanifestcp =  BooleanSetting ("-usemanifestcp", "Utilize the manifest in classpath resolution.")
  val verbose =        BooleanSetting ("-verbose", "Output messages about what the compiler is doing.")
  val version =        BooleanSetting ("-version", "Print product version and exit.")

  // Support passe prefixes of -target values:
  //   - `jvm-` (from back when we also had `msil`)
  //   - `1.` (from back when Java 2 was a possibility)
  private def normalizeTarget(in: String): String = {
    val oldTarget = raw"1\.([5-8])".r
    val oldJvm = raw"jvm-1\.([5-8])".r
    val jvmish = raw"jvm-(\d*)".r
    in match {
      case oldJvm(n) => n
      case oldTarget(n) => n
      case jvmish(n) => n
      case n => n
    }
  }
}

object StandardScalaSettings {
  // not final in case some separately compiled client code wanted to depend on updated values
  val MinTargetVersion = 5
  val MinSupportedTargetVersion = 8
  val MaxTargetVersion = ScalaVersion(javaSpecVersion) match {
    case SpecificScalaVersion(1, minor, _, _) => minor
    case SpecificScalaVersion(major, _, _, _) => major
    case _ => 23
  }
  val MaxSupportedTargetVersion = 8
  val DefaultTargetVersion = "8"

  private val AllTargetVersions = (MinTargetVersion to MaxTargetVersion).map(_.toString).toList
  val AllPermissibleTargetValues: List[String] = AllTargetVersions.flatMap(v => v :: s"jvm-1.$v" :: s"jvm-$v" :: s"1.$v" :: Nil)
}
