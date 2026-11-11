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
    mimaReferenceVersion := Some("2.13.18"),
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

    // KEEP: the CommonErrors object is not a public API
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.generic.CommonErrors"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.generic.CommonErrors$"),

    // KEEP: new jdk 25 method in CharSequence => mixin forwarders
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Predef#ArrayCharSequence.getChars"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Predef#SeqCharSequence.getChars"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.mutable.StringBuilder.getChars"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.ArrayCharSequence.getChars"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.SeqCharSequence.getChars"),

    // scala/scala#11242
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.LazyList$MidEvaluation$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.LazyList$InRace"),

    // scala/scala#11242
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.LazyList"), // superclass change from AbstractSeq to LazyListBase
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.LazyListBase*"), // private[immutable]

    // scala/bug#13183: new private classes in the private[reflect] trait SynchronizedSymbols
    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.runtime.SynchronizedSymbols$SynchronizedStubClassSymbol"),
    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.runtime.SynchronizedSymbols$SynchronizedStubTermSymbol"),
    ProblemFilters.exclude[NewMixinForwarderProblem]("scala.reflect.runtime.SynchronizedSymbols.newStubSymbol"),

    // scala/bug#13202: lub/glb memo tables grouped into one holder, a single thread-local in the private[reflect] trait SynchronizedTypes
    ProblemFilters.exclude[NewMixinForwarderProblem]("scala.reflect.runtime.SynchronizedTypes.lubResults"),
    ProblemFilters.exclude[NewMixinForwarderProblem]("scala.reflect.runtime.SynchronizedTypes.glbResults"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.JavaUniverse.lubGlbCaches"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.JavaUniverse.lubGlbCaches_="),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedTypes.lubGlbCaches"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedTypes.lubGlbCaches_="),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedTypes.lubGlbCaches"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedTypes.lubGlbCaches_="),

    // scala/bug#13203: thread-local override of internal state in the private[reflect] trait SynchronizedTypes
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedTypes.globalGlbDepth*"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.JavaUniverse.globalGlbDepth*"),

    // implementation traits of the reflect internal API (Kinds, Variances, GlbLubs, ...) are components of SymbolTable rather than
    // mixins, so the private[reflect] runtime universe no longer has them as parents, nor their synthetic accessors
    ProblemFilters.exclude[MissingTypesProblem]("scala.reflect.runtime.JavaUniverse"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.reflect.runtime.SymbolTable"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.reflect.runtime.SynchronizedOps"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.reflect.runtime.SynchronizedTypes"),
    ProblemFilters.exclude[InheritedNewAbstractMethodProblem]("scala.reflect.runtime.SymbolTable.scala$reflect$internal$tpe$*"),
    ProblemFilters.exclude[InheritedNewAbstractMethodProblem]("scala.reflect.runtime.SynchronizedOps.scala$reflect$internal$tpe$*"),
    ProblemFilters.exclude[InheritedNewAbstractMethodProblem]("scala.reflect.runtime.SynchronizedTypes.scala$reflect$internal$tpe$*"),
    ProblemFilters.exclude[InheritedNewAbstractMethodProblem]("scala.reflect.runtime.SymbolTable.GlbFailure*"),
    ProblemFilters.exclude[InheritedNewAbstractMethodProblem]("scala.reflect.runtime.SynchronizedOps.GlbFailure*"),
    ProblemFilters.exclude[InheritedNewAbstractMethodProblem]("scala.reflect.runtime.SynchronizedTypes.GlbFailure*"),

    // isCompilerUniverse and hasSynchronizedSymbols are vals in a new mixin of the internal SymbolTable, and universes override their compute methods.
    // runtime.SymbolTable is private[scala]; it only escapes as a parent of JavaUniverse.
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SymbolTable.isCompilerUniverse"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SymbolTable.computeIsCompilerUniverse"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.JavaUniverse.computeIsCompilerUniverse"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SymbolTable.computeHasSynchronizedSymbols"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.JavaUniverse.computeHasSynchronizedSymbols"),

    // scala.reflect.api: ClassSymbolApi's final override of isClass conflicts with Symbol's final isClass, so it is now abstract.
    // Only a Universe implementation can mix ClassSymbolApi in (its self type is ClassSymbol), and scala-reflect is the only one.
    ProblemFilters.exclude[NewMixinForwarderProblem]("scala.reflect.api.Symbols#ClassSymbolApi.isClass"),

    // Final Symbol.info and rawInfo: runtime reflection's locking moves behind hooks, in the private[reflect] trait SynchronizedSymbol
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.info"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.rawInfo"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.synchronizedInfo"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.synchronizedRawInfo"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.scala$reflect$runtime$SynchronizedSymbols$SynchronizedSymbol$$super$info"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.scala$reflect$runtime$SynchronizedSymbols$SynchronizedSymbol$$super$rawInfo"),

    // Final Symbol.tpe_*: runtime reflection's locking moves behind a hook, in the private[reflect] trait SynchronizedTypeSymbol
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedTypeSymbol.tpe_*"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedTypeSymbol.synchronizedTpe"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedTypeSymbol.scala$reflect$runtime$SynchronizedSymbols$SynchronizedTypeSymbol$$super$tpe_*"),
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
