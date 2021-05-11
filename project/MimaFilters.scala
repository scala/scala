package scala.build

import sbt._, Keys._
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.plugin.MimaPlugin, MimaPlugin.autoImport._

object MimaFilters extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val mimaReferenceVersion = settingKey[Option[String]]("Scala version number to run MiMa against")
  }
  import autoImport._

  override val globalSettings = Seq(
    mimaReferenceVersion := Some("2.13.5"),
  )

  val mimaFilters: Seq[ProblemFilter] = Seq[ProblemFilter](
    // KEEP: the reflect internal API isn't public API
    ProblemFilters.exclude[Problem]("scala.reflect.internal.*"),

    // KEEP: java.util.Enumeration.asIterator only exists in later JDK versions (11 at least).  If you build
    // with JDK 11 and run MiMa it'll complain IteratorWrapper isn't forwards compatible with 2.13.0 - but we
    // don't publish the artifact built with JDK 11 anyways
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.convert.JavaCollectionWrappers#IteratorWrapper.asIterator"),

    // for the method this(Long)Unit in class scala.math.BigInt does not have a correspondent in other versions
    // this new constructor is nevertheless private, and can only be called from the BigInt class and its companion
    // object
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.math.BigInt.this"),

    // PR: https://github.com/scala/scala/pull/9336; remove after re-STARR
    ProblemFilters.exclude[MissingTypesProblem]("scala.deprecatedOverriding"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.deprecatedInheritance"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.deprecated"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.annotation.elidable"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.annotation.implicitAmbiguous"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.annotation.implicitNotFound"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.annotation.migration"),

    // when building on a recent JDK, classes implementing `CharSequence` get a mixin forwarder for
    // the `isEmpty` default method that was added in JDK 15
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Predef#SeqCharSequence.isEmpty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Predef#ArrayCharSequence.isEmpty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.ArrayCharSequence.isEmpty"),
  )

  override val buildSettings = Seq(
    mimaFailOnNoPrevious := false, // we opt everything out, knowing we only check library/reflect
  )

  val mimaSettings: Seq[Setting[_]] = Def.settings(
    mimaPreviousArtifacts       := mimaReferenceVersion.value.map(organization.value % name.value % _).toSet,
    mimaCheckDirection          := "both",
    mimaBinaryIssueFilters     ++= mimaFilters,
//  mimaReportSignatureProblems := true, // TODO: enable
  )
}
