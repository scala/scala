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
    mimaReferenceVersion := Some("2.13.15"),
  )

  val mimaFilters: Seq[ProblemFilter] = Seq[ProblemFilter](
    // KEEP: the reflect internal API isn't public API
    ProblemFilters.exclude[Problem]("scala.reflect.internal.*"),

    // KEEP: java.util.Enumeration.asIterator only exists in later JDK versions (11 at least).  If you build
    // with JDK 11 and run MiMa it'll complain IteratorWrapper isn't forwards compatible with 2.13.0 - but we
    // don't publish the artifact built with JDK 11 anyways
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.convert.JavaCollectionWrappers#IteratorWrapper.asIterator"),

    // KEEP: when building on a recent JDK, classes implementing `CharSequence` get a mixin forwarder for
    // the `isEmpty` default method that was added in JDK 15
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Predef#SeqCharSequence.isEmpty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Predef#ArrayCharSequence.isEmpty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.ArrayCharSequence.isEmpty"),

    // KEEP: make use of CompletionStage#handle to get a better performance than CompletionStage#whenComplete.
    ProblemFilters.exclude[MissingTypesProblem]("scala.concurrent.impl.FutureConvertersImpl$P"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.concurrent.impl.FutureConvertersImpl#P.andThen"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.concurrent.impl.FutureConvertersImpl#P.apply"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.concurrent.impl.FutureConvertersImpl#P.andThen"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.concurrent.impl.FutureConvertersImpl#P.accept"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.concurrent.impl.FutureConvertersImpl#P.andThen"),

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
