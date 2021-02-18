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
    mimaReferenceVersion := Some("2.13.4"),
  )

  val mimaFilters: Seq[ProblemFilter] = Seq[ProblemFilter](
    // KEEP: we don't the reflect internal API isn't public API
    ProblemFilters.exclude[Problem]("scala.reflect.internal.*"),

    // KEEP: java.util.Enumeration.asIterator only exists in later JDK versions (11 at least).  If you build
    // with JDK 11 and run MiMa it'll complain IteratorWrapper isn't forwards compatible with 2.13.0 - but we
    // don't publish the artifact built with JDK 11 anyways
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.convert.JavaCollectionWrappers#IteratorWrapper.asIterator"),

    // #9425 Node is private[collection]
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.mutable.HashMap#Node.foreachEntry"),

    // #9487
    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.ClassTag$cache$"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree#Tree.redWithRight"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree#Tree.redWithLeftRight"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree#Tree.blackWithLeftRight"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree#Tree.redWithLeft"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree.partitionKeys"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree.filterKeys"),
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
