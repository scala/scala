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
    mimaReferenceVersion := Some("2.13.6"),
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

    // #9425 Node is private[collection]
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.mutable.HashMap#Node.foreachEntry"),

    // Fixes for scala/bug#12009
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.mutable.ArrayBufferView.this"),                  // private[mutable]
    ProblemFilters.exclude[FinalClassProblem]("scala.collection.IndexedSeqView$IndexedSeqViewIterator"),                  // private[collection]
    ProblemFilters.exclude[FinalClassProblem]("scala.collection.IndexedSeqView$IndexedSeqViewReverseIterator"),           // private[collection]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView"),                        // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$"),                       // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$CheckedIterator"),        // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$CheckedReverseIterator"), // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Id"),                     // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Appended"),               // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Prepended"),              // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Concat"),                 // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Take"),                   // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$TakeRight"),              // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Drop"),                   // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$DropRight"),              // private[mutable]
    ProblemFilters.exclude[MissingClassProblem](s"scala.collection.mutable.CheckedIndexedSeqView$$Map"),                  // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Reverse"),                // private[mutable]
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.CheckedIndexedSeqView$Slice"),                  // private[mutable]

    // #8835
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedOps#SynchronizedBaseTypeSeq.scala$reflect$runtime$SynchronizedOps$SynchronizedBaseTypeSeq$$super$maxDepthOfElems"),

    // this is an internal class and adding a final override here should not be a problem
    ProblemFilters.exclude[FinalMethodProblem]("scala.concurrent.impl.Promise#DefaultPromise.zipWith"),

    // private[scala] Internal API
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.reflect.io.FileZipArchive#LeakyEntry.this"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.reflect.io.FileZipArchive#LeakyEntry.this"),
    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.io.FileZipArchive$zipFilePool$"),

    // #9727
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.concurrent.TrieMap.filterInPlaceImpl"),                                      // private[collection]
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.concurrent.TrieMap.mapValuesInPlaceImpl"),                                   // private[collection]
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.convert.JavaCollectionWrappers#JConcurrentMapWrapper.filterInPlaceImpl"),    // private[collection]
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.convert.JavaCollectionWrappers#JConcurrentMapWrapper.mapValuesInPlaceImpl"), // private[collection]

    // #9733
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.concurrent.TrieMap$RemovalPolicy$"),                                        // private[concurrent]
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.convert.JavaCollectionWrappers#JConcurrentMapWrapper.removeRefEq"),  // private[collection]
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.convert.JavaCollectionWrappers#JConcurrentMapWrapper.replaceRefEq"), // private[collection]

    // #9741
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.SeqMap$SeqMapBuilderImpl"), // private[SeqMap]

    // #9752
    ProblemFilters.exclude[MissingTypesProblem]("scala.reflect.ClassTag$cache$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.ModuleSerializationProxy$"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.reflect.runtime.JavaMirrors$JavaMirror$typeTagCache$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.ClassValueCompat"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.ClassValueCompat$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.ClassValueCompat$ClassValueInterface"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.ClassValueCompat$JavaClassValue"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.ClassValueCompat$FallbackClassValue"),

    // #9782
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.mutable.ArrayBuffer$SplitInfo"), // ArrayBuffer#private[this]
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
