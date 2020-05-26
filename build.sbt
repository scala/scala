/*
 * The new, sbt-based build definition for Scala.
 *
 * What you see below is very much work-in-progress. The following features are implemented:
 *   - Compiling all classses for the compiler and library ("compile" in the respective subprojects)
 *   - Running JUnit ("junit/test"), ScalaCheck ("scalacheck/test"), and partest ("test/it:test") tests
 *   - Creating build/quick with all compiled classes and launcher scripts ("dist/mkQuick")
 *   - Creating build/pack with all JARs and launcher scripts ("dist/mkPack")
 *   - Building all scaladoc sets ("doc")
 *   - Publishing ("publishDists" and standard sbt tasks like "publish" and "publishLocal")
 *
 * You'll notice that this build definition is much more complicated than your typical sbt build.
 * The main reason is that we are not benefiting from sbt's conventions when it comes project
 * layout. For that reason we have to configure a lot more explicitly. I've tried explain in
 * comments the less obvious settings.
 *
 * This nicely leads me to explaining goal and non-goals of this build definition. Goals are:
 *
 *   - to be easy to tweak it in case a bug or small inconsistency is found
 *   - to be super explicit about any departure from standard sbt settings
 *   - to be readable and not necessarily succinct
 *   - to provide the nicest development experience for people hacking on Scala
 *   - originally, to mimic Ant's behavior as closely as possible, so the
 *     sbt and Ant builds could be maintained in parallel. the Ant build
 *     has now been removed, so we are now free to depart from that history.
 *
 * Non-goals are:
 *
 *   - to have the shortest sbt build definition possible
 *   - to remove irregularities from our build process right away
 *     (but let's keep making gradual progress on this)
 *   - to modularize the Scala compiler or library further
 */

import sbt.{Global, TestResult}

import scala.build._
import VersionUtil._

// Scala dependencies:
val scalaSwingDep                = scalaDep("org.scala-lang.modules", "scala-swing")
val scalaXmlDep                  = scalaDep("org.scala-lang.modules", "scala-xml")
val scalaParserCombinatorsDep    = scalaDep("org.scala-lang.modules", "scala-parser-combinators")
val partestDep                   = scalaDep("org.scala-lang.modules", "scala-partest",              versionProp = "partest")

// Non-Scala dependencies:
val junitDep          = "junit"                  % "junit"           % "4.12"
val junitInterfaceDep = "com.novocode"           % "junit-interface" % "0.11"                            % "test"
val scalacheckDep     = "org.scalacheck"         % "scalacheck_2.12" % "1.14.3"                          % "test"
val jolDep            = "org.openjdk.jol"        % "jol-core"        % "0.9"
val asmDep            = "org.scala-lang.modules" % "scala-asm"       % versionProps("scala-asm.version")
val jlineDep          = "jline"                  % "jline"           % versionProps("jline.version")
val jansiDep          = "org.fusesource.jansi"   % "jansi"           % "1.12"
val antDep            = "org.apache.ant"         % "ant"             % "1.9.4"

/** Publish to ./dists/maven-sbt, similar to the Ant build which publishes to ./dists/maven. This
  * can be used to compare the output of the sbt and Ant builds during the transition period. Any
  * real publishing should be done with sbt's standard `publish` task. */
lazy val publishDists = taskKey[Unit]("Publish to ./dists/maven-sbt.")

lazy val publishSettings : Seq[Setting[_]] = Seq(
  publishDists := {
    val artifacts = (packagedArtifacts in publish).value
    val ver = VersionUtil.versionProperties.value.canonicalVersion
    val log = streams.value.log
    val mappings = artifacts.toSeq.map { case (a, f) =>
      val typeSuffix = a.`type` match {
        case "pom" => "-pom.xml"
        case "jar" => ".jar"
        case "doc" => "-docs.jar"
        case tpe => s"-$tpe.${a.extension}"
      }
      val to = file("dists/maven-sbt") / ver / a.name / (a.name + typeSuffix)
      log.info(s"Publishing $f to $to")
      (f, to)
    }
    IO.copy(mappings)
  },
  credentials ++= {
    val file = Path.userHome / ".credentials"
    if (file.exists && !file.isDirectory) List(Credentials(file))
    else Nil
  },
  // Add a "default" Ivy configuration because sbt expects the Scala distribution to have one:
  ivyConfigurations += Configuration.of("Default", "default", "Default", true, Vector(Configurations.Runtime), true),
  publishMavenStyle := true
)

// Set the version number: We use the two settings `baseVersion` and `baseVersionSuffix` to compute all versions
// (canonical, Maven, OSGi). See VersionUtil.versionPropertiesImpl for details. The standard sbt `version` setting
// should not be set directly. It is the same as the Maven version and derived automatically from `baseVersion` and
// `baseVersionSuffix`.
globalVersionSettings
baseVersion in Global       := "2.12.11"
baseVersionSuffix in Global := "SNAPSHOT"
organization in ThisBuild   := "org.scala-lang"
homepage in ThisBuild       := Some(url("https://www.scala-lang.org"))
startYear in ThisBuild      := Some(2002)
licenses in ThisBuild       += (("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0")))
headerLicense in ThisBuild  := Some(HeaderLicense.Custom(
  s"""Scala (${(homepage in ThisBuild).value.get})
     |
     |Copyright EPFL and Lightbend, Inc.
     |
     |Licensed under Apache License 2.0
     |(http://www.apache.org/licenses/LICENSE-2.0).
     |
     |See the NOTICE file distributed with this work for
     |additional information regarding copyright ownership.
     |""".stripMargin
))

Global / mimaReferenceVersion := Some("2.12.0")

import com.typesafe.tools.mima.core._
val mimaFilterSettings = Seq {
  mimaBinaryIssueFilters ++= Seq(
    ProblemFilters.exclude[IncompatibleSignatureProblem]("*"),
    ProblemFilters.exclude[InaccessibleMethodProblem]("java.lang.Object.<clinit>"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Vector.debug"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.VectorBuilder.debug"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.VectorPointer.debug"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.VectorIterator.debug"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.mutable.OpenHashMap.nextPositivePowerOfTwo"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.mutable.HashTable.powerOfTwo"),
    ProblemFilters.exclude[MissingClassProblem]("scala.concurrent.impl.ExecutionContextImpl$AdaptedForkJoinTask"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.sys.process.ProcessImpl#CompoundProcess.getExitValue"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.sys.process.ProcessImpl#CompoundProcess.futureValue"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.sys.process.ProcessImpl#CompoundProcess.futureThread"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.mutable.HashTable.nextPositivePowerOfTwo"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.util.hashing.MurmurHash3.wrappedBytesHash"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.util.hashing.MurmurHash3.wrappedArrayHash"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap.contains0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.contains0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashMapCollision1.contains0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashMap1.contains0"),

    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.showAsInfix$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.showAsInfix"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.util.PropertiesTrait.coloredOutputEnabled"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.util.Properties.coloredOutputEnabled"),

    // https://github.com/scala/scala/pull/6101
    ProblemFilters.exclude[MissingTypesProblem]("scala.runtime.LazyRef"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.runtime.LazyDouble"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.runtime.LazyChar"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.runtime.LazyUnit"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.runtime.LazyShort"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.runtime.LazyInt"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.runtime.LazyByte"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.runtime.LazyLong"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.runtime.LazyBoolean"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.runtime.LazyFloat"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyRef.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyDouble.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyChar.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyUnit.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyShort.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyInt.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyByte.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyLong.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyBoolean.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.LazyFloat.serialVersionUID"),

    // https://github.com/scala/scala/pull/6138
    ProblemFilters.exclude[MissingFieldProblem]("scala.collection.immutable.Map#EmptyMap.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.collection.immutable.Map#Map1.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.collection.immutable.Map#Map2.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.collection.immutable.Map#Map3.serialVersionUID"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.collection.immutable.Map#Map4.serialVersionUID"),
    ProblemFilters.exclude[DirectAbstractMethodProblem]("scala.collection.GenTraversableOnce.toList"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap.foreachEntry"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.Map$HashCodeAccumulator"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.foreachEntry"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.ListMap.foreachEntry"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashMap1.foreachEntry"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashMapCollision1.foreachEntry"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.util.hashing.MurmurHash3.product2Hash"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.util.hashing.MurmurHash3.emptyMapHash"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.HashMap$HashMapKeys"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.HashMap$HashMapValues"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree.*"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.RedBlackTree$NList"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.RedBlackTree$NList$"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.RedBlackTree$EntriesIterator"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.RedBlackTree$ValuesIterator"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.RedBlackTree$TreeIterator"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.generic.SortedMapFactory#SortedMapCanBuildFrom.ordering"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree#TreeIterator.ordering"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.TreeMap.removeAllImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.TreeMap.filterImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.TreeSet.tree"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.TreeSet.addAllTreeSetImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.TreeSet.addAllImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.TreeSet.removeAll"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.TreeSet.filterImpl"),

    ProblemFilters.exclude[MissingClassProblem]("scala.collection.package$WrappedCanBuildFrom"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.HashMap$HashMapBuilder"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#Merger.retainIdentical"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.size0"),

    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.collection.immutable.HashSet#HashSetCollision1.union0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet#HashSetCollision1.union0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet#HashSetCollision1.this"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.collection.immutable.HashSet.union0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet.union0"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.collection.immutable.HashSet#HashTrieSet.union0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet#HashTrieSet.union0"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.collection.immutable.HashSet#HashSet1.union0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet#HashSet1.union0"),
    ProblemFilters.exclude[FinalMethodProblem]("scala.collection.immutable.HashSet#LeafHashSet.hash"),
    ProblemFilters.exclude[DirectAbstractMethodProblem]("scala.collection.immutable.HashSet#LeafHashSet.hash"),
    ProblemFilters.exclude[ReversedAbstractMethodProblem]("scala.collection.immutable.HashSet#LeafHashSet.hash"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet#LeafHashSet.this"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet.elemHashCode"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet.computeHash"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet.improve"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet#HashTrieSet.bitmap"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet#HashTrieSet.bitmap_="),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet#HashTrieSet.elems_="),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet#HashTrieSet.size0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashSet#HashTrieSet.size0_="),
    ProblemFilters.exclude[FinalMethodProblem]("scala.collection.immutable.HashSet#HashTrieSet.size"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.HashSet$HashSetBuilder"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.Set$SetBuilderImpl"),

    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.RedBlackTree$EqualsIterator"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree#TreeIterator.findLeftMostOrPopOnEmpty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree#TreeIterator.popNext"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree#TreeIterator.lookahead"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree#TreeIterator.lookahead_="),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree#TreeIterator.goRight"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.RedBlackTree#TreeIterator.stackOfNexts"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.TreeMap.this"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.TreeMap$TreeMapBuilder$adder$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.TreeMap$TreeMapBuilder"),

    // Some static forwarder changes detected after a MiMa upgrade.
    // e.g. static method apply(java.lang.Object)java.lang.Object in class scala.Symbol does not have a correspondent in current version
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.Symbol.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.sys.process.Process.Future"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.sys.process.Process.Spawn"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.takeWhile"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.slice"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.drop"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.take"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.seq"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.seq"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.seq"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.seq"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.toSeq"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.reverse"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.toCollection"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.thisCollection"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.seq"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.andThen"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.view"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.view"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.dropRight"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.takeRight"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.takeWhile"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.drop"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.take"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.slice"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.head"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.thisCollection"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.seq"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.reversed"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.view"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.view"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.dropWhile"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.tail"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.thisCollection"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Nil.seq"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.List.empty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Queue.empty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Vector.empty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Stream.iterate"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Stream.range"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Stream.tabulate"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Stream.fill"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Stream.empty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.mutable.LinkedList.empty"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.util.Properties.scalaProps"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.util.Properties.propFilename"),

    // https://github.com/scala/scala/pull/8630
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap.transformImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.transformImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashMap1.transformImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashMapCollision1.transformImpl"),

    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.HasForeachEntry"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.TreeMap.foreachEntry"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Map#EmptyMap.foreachEntry"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Map#Map1.foreachEntry"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Map#Map2.foreachEntry"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Map#Map3.foreachEntry"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Map#Map4.foreachEntry"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.ListMap"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.TreeMap"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.collection.immutable.TreeMap.serialVersionUID"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.HashMap"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.HashMap$HashMap1"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.HashMap$HashTrieMap"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.HashMap$HashMapCollision1"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.ListMap$Node"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.ListMap$EmptyListMap$"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.HashMap$EmptyHashMap$"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.Map$Map1"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.Map$Map2"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.Map$Map3"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.Map$Map4"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.collection.immutable.Map$EmptyMap$"),

    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.TreeSet$TreeSetBuilder"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.TreeSet.this"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.TreeSet$TreeSetBuilder$adder$"),

    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.collection.immutable.SortedSet.scala$collection$immutable$SortedSet$$super=uals"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.collection.immutable.SortedMap.scala$collection$immutable$SortedMap$$super=uals"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap.elemHashCode"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap.improve"),
    ProblemFilters.exclude[FinalMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.bitmap"),
    ProblemFilters.exclude[FinalMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.elems"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.Map$MapBuilderImpl"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.HashMap$HashMapBuilder"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.bitmap0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.bitmap0_="),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.elems0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.elems0_="),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.size0_="),
    ProblemFilters.exclude[FinalMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.size"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.generic.SortedMapFactory#SortedMapCanBuildFrom.factory"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.generic.SortedSetFactory#SortedSetCanBuildFrom.factory"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.generic.SortedSetFactory#SortedSetCanBuildFrom.ordering"),

    //
    // scala-reflect
    //
    ProblemFilters.exclude[Problem]("scala.reflect.internal.*"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.reflect.runtime.JavaMirrors#JavaMirror.unpickleClass"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.reflect.runtime.SymbolLoaders#TopClassCompleter.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedOps#SynchronizedBaseTypeSeq.lateMap"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.scala$reflect$runtime$SynchronizedSymbols$SynchronizedSymbol$$super$exists"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.scala$reflect$runtime$SynchronizedSymbols$SynchronizedSymbol$$super$getFlag"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.scala$reflect$runtime$SynchronizedSymbols$SynchronizedSymbol$$super$privateWithin"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.scala$reflect$runtime$SynchronizedSymbols$SynchronizedSymbol$$super$annotations"),
    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.io.IOStats"),
    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.io.IOStats$"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.reflect.runtime.JavaUniverse"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.io.ZipArchive.close"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.reflect.io.ZipArchive.getDir"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("scala.reflect.io.FileZipArchive.allDirs"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.scala$reflect$runtime$SynchronizedSymbols$SynchronizedSymbol$$super$typeConstructor"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedTypes.scala$reflect$runtime$SynchronizedTypes$$super$defineNormalized"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedTypes.defineNormalized"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.JavaUniverse.defineNormalized"),

    ProblemFilters.exclude[Problem]("scala.reflect.internal.*"),

    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.reflect.runtime.JavaMirrors#JavaMirror.unpickleClass"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.reflect.runtime.SymbolLoaders#TopClassCompleter.this"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.Settings.Yvirtpatmat"),
    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.io.PlainNioFile"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedOps.newMappedBaseTypeSeq"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.JavaUniverse.newMappedBaseTypeSeq"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.JavaUniverse.statistics"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.JavaMirrors#JavaMirror#JavaAnnotationProxy.transformArgs"),

    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.io.FileZipArchive$LazyEntry"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.ZipArchive.closeZipFile"),
    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.io.FileZipArchive$LeakyEntry"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.exists"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.getFlag"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.privateWithin"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.annotations"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.Settings.isScala213"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.FileZipArchive.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.ZipArchive.this"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.ZipArchive.getDir"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.FileZipArchive.allDirsByDottedName"),

    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.io.RootPath"),
    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.io.RootPath$"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.URLZipArchive.close"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.FileZipArchive.close"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.ManifestResources.close"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.ZipArchive.close"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.JavaMirrors#JavaMirror.typeTag"),
    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.runtime.JavaMirrors$JavaMirror$typeTagCache$"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.api.TypeTags.TypeTagImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.api.Universe.TypeTagImpl"),

    ProblemFilters.exclude[MissingClassProblem]("scala.reflect.macros.Attachments$"),

    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.reflect.io.ZipArchive.getDir"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("scala.reflect.io.FileZipArchive.allDirs"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedSymbols#SynchronizedSymbol.typeConstructor"),

    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.SynchronizedTypes.scala$reflect$runtime$SynchronizedTypes$$super$defineNormalized"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.SynchronizedTypes.defineNormalized"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.JavaUniverse.defineNormalized"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.AbstractFile.unsafeToByteArray"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.VirtualFile.unsafeToByteArray"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.ZipArchive#Entry.unsafeToByteArray"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.NoAbstractFile.unsafeToByteArray"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.NoAbstractFile.seq"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.NoAbstractFile.view"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.io.NoAbstractFile.view"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Map#Map1.filterImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Map#Map2.filterImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Map#Map3.filterImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Map#Map4.filterImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Set#Set1.filterImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Set#Set2.filterImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Set#Set3.filterImpl"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.Set#Set4.filterImpl"),
    ProblemFilters.exclude[MissingClassProblem]("scala.collection.immutable.Set$SetNIterator"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.reflect.runtime.ReflectSetup.phaseWithId"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.ReflectSetup.scala$reflect$runtime$ReflectSetup$_setter_$phaseWithId_="),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.reflect.runtime.ReflectSetup.phaseWithId"),

    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap.getOrElse0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashTrieMap.getOrElse0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashMap1.getOrElse0"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.collection.immutable.HashMap#HashMapCollision1.getOrElse0"),

    ProblemFilters.exclude[MissingClassProblem]("scala.math.Ordering$IterableOrdering"),
    ProblemFilters.exclude[MissingClassProblem]("scala.math.Ordering$Tuple4Ordering"),
    ProblemFilters.exclude[MissingClassProblem]("scala.math.Ordering$Tuple9Ordering"),
    ProblemFilters.exclude[MissingClassProblem]("scala.math.Ordering$Reverse"),
    ProblemFilters.exclude[MissingClassProblem]("scala.math.Ordering$Tuple2Ordering"),
    ProblemFilters.exclude[MissingClassProblem]("scala.math.Ordering$Tuple3Ordering"),
    ProblemFilters.exclude[MissingClassProblem]("scala.math.Ordering$Tuple7Ordering"),
    ProblemFilters.exclude[MissingClassProblem]("scala.math.Ordering$Tuple6Ordering"),
    ProblemFilters.exclude[MissingClassProblem]("scala.math.Ordering$Tuple8Ordering"),
    ProblemFilters.exclude[MissingClassProblem]("scala.math.Ordering$Tuple5Ordering"),
  )
}

scalaVersion in Global         := versionProps("starr.version")

lazy val instanceSettings = Seq[Setting[_]](
  // we don't cross build Scala itself
  crossPaths := false,
  // do not add Scala library jar as a dependency automatically
  autoScalaLibrary := false,
  // Avoid circular dependencies for scalaInstance (see https://github.com/sbt/sbt/issues/1872)
  managedScalaInstance := false,
  scalaInstance := {
    val s = (scalaInstance in bootstrap).value
    // sbt claims that s.isManagedVersion is false even though s was resolved by Ivy
    // We create a managed copy to prevent sbt from putting it on the classpath where we don't want it
    if(s.isManagedVersion) s else {
      import sbt.internal.inc.ScalaInstance
      val s2 = new ScalaInstance(s.version, s.loader, s.loaderLibraryOnly, s.libraryJars, s.compilerJar, s.allJars, Some(s.actualVersion))
      assert(s2.isManagedVersion)
      s2
    }
  },
  // sbt endeavours to align both scalaOrganization and scalaVersion
  // in the Scala artefacts, for example scala-library and scala-compiler.
  // This doesn't work in the scala/scala build because the version of scala-library and the scalaVersion of
  // scala-library are correct to be different. So disable overriding.
  scalaModuleInfo ~= (_ map (_ withOverrideScalaVersion false)),
  Quiet.silenceScalaBinaryVersionWarning
)

lazy val commonSettings = instanceSettings ++ clearSourceAndResourceDirectories ++ publishSettings ++ Seq[Setting[_]](
  // we always assume that Java classes are standalone and do not have any dependency
  // on Scala classes
  compileOrder := CompileOrder.JavaThenScala,
  javacOptions in Compile ++= Seq("-g", "-source", "1.8", "-target", "1.8", "-Xlint:unchecked"),
  unmanagedJars in Compile := Seq.empty,  // no JARs in version control!
  sourceDirectory in Compile := baseDirectory.value,
  unmanagedSourceDirectories in Compile := List(baseDirectory.value),
  unmanagedResourceDirectories in Compile += (baseDirectory in ThisBuild).value / "src" / thisProject.value.id,
  sourcesInBase := false,
  scalaSource in Compile := (sourceDirectory in Compile).value,
  javaSource in Compile := (sourceDirectory in Compile).value,
  // resources are stored along source files in our current layout
  resourceDirectory in Compile := (sourceDirectory in Compile).value,
  // each subproject has to ask specifically for files they want to include
  includeFilter in unmanagedResources in Compile := NothingFilter,
  target := (baseDirectory in ThisBuild).value / "target" / thisProject.value.id,
  classDirectory in Compile := buildDirectory.value / "quick/classes" / thisProject.value.id,
  target in Compile in doc := buildDirectory.value / "scaladoc" / thisProject.value.id,
  // given that classDirectory and doc target are overridden to be _outside_ of target directory, we have
  // to make sure they are being cleaned properly
  cleanFiles += (classDirectory in Compile).value,
  cleanFiles += (target in Compile in doc).value,
  fork in run := true,
  scalacOptions in Compile += "-Ywarn-unused:imports",
  scalacOptions in Compile in doc ++= Seq(
    "-doc-footer", "epfl",
    "-diagrams",
    "-implicits",
    "-groups",
    "-doc-version", versionProperties.value.canonicalVersion,
    "-doc-title", description.value,
    "-sourcepath", (baseDirectory in ThisBuild).value.toString,
    "-doc-source-url", s"https://github.com/scala/scala/tree/${versionProperties.value.githubTree}€{FILE_PATH_EXT}#L€{FILE_LINE}"
  ),
  setIncOptions,
  apiURL := Some(url("https://www.scala-lang.org/api/" + versionProperties.value.mavenVersion + "/")),
  pomIncludeRepository := { _ => false },
  pomExtra := {
    <scm>
      <connection>scm:git:git://github.com/scala/scala.git</connection>
      <url>https://github.com/scala/scala.git</url>
    </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/scala/bug/issues</url>
      </issueManagement>
      <developers>
        <developer>
          <id>lamp</id>
          <name>LAMP/EPFL</name>
        </developer>
        <developer>
          <id>Lightbend</id>
          <name>Lightbend, Inc.</name>
        </developer>
      </developers>
  },
  headerLicense := (headerLicense in ThisBuild).value,
  // Remove auto-generated manifest attributes
  packageOptions in Compile in packageBin := Seq.empty,
  packageOptions in Compile in packageSrc := Seq.empty,

  // Lets us CTRL-C partest without exiting SBT entirely
  cancelable in Global := true,

  // Don't pick up source files from the project root.
  sourcesInBase := false,

  // Don't log process output (e.g. of forked `compiler/runMain ...Main`), just pass it
  // directly to stdout
  outputStrategy in run := Some(StdoutOutput)
) ++ removePomDependencies ++ setForkedWorkingDirectory

/** Extra post-processing for the published POM files. These are needed to create POMs that
  * are equivalent to the ones from the Ant build. In the long term this should be removed and
  * POMs, scaladocs, OSGi manifests, etc. should all use the same metadata. */
def fixPom(extra: (String, scala.xml.Node)*): Setting[_] = {
  /** Find elements in an XML document by a simple XPath and replace them */
  def fixXML(n: scala.xml.Node, repl: Map[String, scala.xml.Node]): scala.xml.Node = {
    def f(n: scala.xml.Node, p: String): scala.xml.Node = n match {
      case e: scala.xml.Elem =>
        val pp = p + "/" + e.label
        repl.get(pp) match {
          case Some(xml) => xml
          case None => e.copy(child = e.child.map(ch => f(ch, pp)))
        }
      case n => n
    }
    f(n, "")
  }
  pomPostProcess := { n => fixXML(pomPostProcess.value.apply(n), Map(
    "/project/organization" ->
      <organization>
        <name>LAMP/EPFL</name>
        <url>https://lamp.epfl.ch/</url>
      </organization>,
    "/project/url" -> <url>https://www.scala-lang.org/</url>
  ) ++ extra) }
}

val pomDependencyExclusions =
  settingKey[Seq[(String, String)]]("List of (groupId, artifactId) pairs to exclude from the POM and ivy.xml")

pomDependencyExclusions in Global := Nil

/** Remove unwanted dependencies from the POM and ivy.xml. */
lazy val removePomDependencies: Seq[Setting[_]] = Seq(
  pomPostProcess := { n =>
    val n2 = pomPostProcess.value.apply(n)
    val deps = pomDependencyExclusions.value
    import scala.xml._
    import scala.xml.transform._
    new RuleTransformer(new RewriteRule {
      override def transform(node: Node) = node match {
        case e: Elem if e.label == "dependency" &&
            deps.exists { case (g, a) =>
              e.child.contains(<groupId>{g}</groupId>) &&
                (e.child.contains(<artifactId>{a}</artifactId>) || e.child.contains(<artifactId>{a + "_" + scalaBinaryVersion.value}</artifactId>))
            } => Seq.empty
        case n => Seq(n)
      }
    }).transform(Seq(n2)).head
  },
  deliverLocal := {
    import scala.xml._
    import scala.xml.transform._
    val f = deliverLocal.value
    val deps = pomDependencyExclusions.value
    val e = new RuleTransformer(new RewriteRule {
      override def transform(node: Node) = node match {
        case e: Elem if e.label == "dependency" && {
          val org = e.attribute("org").getOrElse("").toString
          val name = e.attribute("name").getOrElse("").toString
          deps.exists { case (g, a) =>
             org == g && (name == a || name == (a + "_" + scalaBinaryVersion.value))
          }
        } => Seq.empty
        case n => Seq(n)
      }
    }).transform(Seq(XML.loadFile(f))).head
    XML.save(f.getAbsolutePath, e, xmlDecl = true)
    f
  }
)

val disableDocs = Seq[Setting[_]](
  sources in (Compile, doc) := Seq.empty,
  publishArtifact in (Compile, packageDoc) := false
)

val disablePublishing = Seq[Setting[_]](
  publishArtifact := false,
  // The above is enough for Maven repos but it doesn't prevent publishing of ivy.xml files
  publish := {},
  publishLocal := {},
  whitesourceIgnore := true
)

lazy val setJarLocation: Setting[_] =
  artifactPath in packageBin in Compile := {
    // two lines below are copied over from sbt's sources:
    // https://github.com/sbt/sbt/blob/0.13/main/src/main/scala/sbt/Defaults.scala#L628
    //val resolvedScalaVersion = ScalaVersion((scalaVersion in artifactName).value, (scalaBinaryVersion in artifactName).value)
    //val resolvedArtifactName = artifactName.value(resolvedScalaVersion, projectID.value, artifact.value)
    // if you would like to get a jar with version number embedded in it (as normally sbt does)
    // uncomment the other definition of the `resolvedArtifactName`
    val resolvedArtifact = artifact.value
    val resolvedArtifactName = s"${resolvedArtifact.name}.${resolvedArtifact.extension}"
    buildDirectory.value / "pack/lib" / resolvedArtifactName
  }
lazy val scalaSubprojectSettings: Seq[Setting[_]] = commonSettings :+ setJarLocation

def filterDocSources(ff: FileFilter): Seq[Setting[_]] = Seq(
  sources in (Compile, doc) ~= (_.filter(ff.accept)),
  // Excluded sources may still be referenced by the included sources, so we add the compiler
  // output to the scaladoc classpath to resolve them. For the `library` project this is
  // always required because otherwise the compiler cannot even initialize Definitions without
  // binaries of the library on the classpath. Specifically, we get this error:
  // (library/compile:doc) scala.reflect.internal.FatalError: package class scala does not have a member Int
  dependencyClasspath in (Compile, doc) += (classDirectory in Compile).value,
  doc in Compile := (doc in Compile).dependsOn(compile in Compile).value
)

def regexFileFilter(s: String): FileFilter = new FileFilter {
  val pat = s.r.pattern
  def accept(f: File) = pat.matcher(f.getAbsolutePath.replace('\\', '/')).matches()
}

def setForkedWorkingDirectory: Seq[Setting[_]] = {
  // When we fork subprocesses, use the base directory as the working directory.
  // This“ enables `sbt> partest test/files/run/t1.scala` or `sbt> scalac sandbox/test.scala`
  val setting = (forkOptions in Compile) := (forkOptions in Compile).value.withWorkingDirectory((baseDirectory in ThisBuild).value)
  setting ++ inTask(run)(setting)
}

// This project provides the STARR scalaInstance for bootstrapping
lazy val bootstrap = project in file("target/bootstrap")

lazy val library = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.library"))
  .settings(
    name := "scala-library",
    description := "Scala Standard Library",
    compileOrder := CompileOrder.Mixed, // needed for JFunction classes in scala.runtime.java8
    scalacOptions in Compile ++= Seq[String]("-sourcepath", (scalaSource in Compile).value.toString),
    scalacOptions in Compile in doc ++= {
      val libraryAuxDir = (baseDirectory in ThisBuild).value / "src/library-aux"
      Seq(
        "-doc-no-compile", libraryAuxDir.toString,
        "-skip-packages", "scala.concurrent.impl",
        "-doc-root-content", (sourceDirectory in Compile).value + "/rootdoc.txt"
      )
    },
    includeFilter in unmanagedResources in Compile := "*.tmpl" | "*.xml" | "*.js" | "*.css" | "rootdoc.txt",
    // Include *.txt files in source JAR:
    mappings in Compile in packageSrc ++= {
      val base = (unmanagedResourceDirectories in Compile).value
      base ** "*.txt" pair Path.relativeTo(base)
    },
    Osgi.headers += "Import-Package" -> "sun.misc;resolution:=optional, *",
    Osgi.jarlist := true,
    fixPom(
      "/project/name" -> <name>Scala Library</name>,
      "/project/description" -> <description>Standard library for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    // Remove the dependency on "forkjoin" from the POM because it is included in the JAR:
    pomDependencyExclusions += ((organization.value, "forkjoin")),
    mimaPreviousArtifacts := mimaReferenceVersion.value.map(organization.value % name.value % _).toSet,
    mimaCheckDirection := "both",
    mimaFilterSettings,
  )
  .settings(filterDocSources("*.scala" -- (regexFileFilter(".*/runtime/.*\\$\\.scala") ||
                                           regexFileFilter(".*/runtime/ScalaRunTime\\.scala") ||
                                           regexFileFilter(".*/runtime/StringAdd\\.scala"))))

lazy val reflect = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.reflect"))
  .settings(mimaFilterSettings)
  .settings(
    name := "scala-reflect",
    description := "Scala Reflection Library",
    Osgi.bundleName := "Scala Reflect",
    scalacOptions in Compile in doc ++= Seq(
      "-skip-packages", "scala.reflect.macros.internal:scala.reflect.internal:scala.reflect.io"
    ),
    Osgi.headers +=
      "Import-Package" -> ("scala.*;version=\"${range;[==,=+);${ver}}\","+
                           "scala.tools.nsc;resolution:=optional;version=\"${range;[==,=+);${ver}}\","+
                           "*"),
    fixPom(
      "/project/name" -> <name>Scala Compiler</name>,
      "/project/description" -> <description>Compiler for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    mimaPreviousArtifacts := mimaReferenceVersion.value.map(organization.value % name.value % _).toSet,
    mimaCheckDirection := "both",
    mimaFilterSettings,
  )
  .dependsOn(library)

lazy val compilerOptionsExporter = Project("compilerOptionsExporter", file(".") / "src" / "compilerOptionsExporter")
  .dependsOn(compiler, reflect, library)
  .settings(clearSourceAndResourceDirectories)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.9.7",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.9.7",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.7",
      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.9.7",
      "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.7"
    )
  )

lazy val compiler = configureAsSubproject(project)
  .settings(generatePropertiesFileSettings)
  .settings(generateBuildCharacterFileSettings)
  .settings(Osgi.settings)
  .settings(AutomaticModuleName.settings("scala.tools.nsc"))
  .settings(
    name := "scala-compiler",
    description := "Scala Compiler",
    libraryDependencies ++= Seq(antDep, asmDep),
    // These are only needed for the POM. (And, note that the jansi dependency is a fiction
    // for WhiteSource purposes; the JLine JAR contains a shaded jansi, but WhiteSource
    // won't know about that unless we tell it.)
    libraryDependencies ++= Seq(scalaXmlDep, jlineDep % "optional", jansiDep % "optional"),
    buildCharacterPropertiesFile := (resourceManaged in Compile).value / "scala-buildcharacter.properties",
    resourceGenerators in Compile += generateBuildCharacterPropertiesFile.map(file => Seq(file)).taskValue,
    // this a way to make sure that classes from interactive and scaladoc projects
    // end up in compiler jar. note that we need to use LocalProject references
    // (with strings) to deal with mutual recursion
    products in Compile in packageBin :=
      (products in Compile in packageBin).value ++
        Seq((dependencyClasspath in Compile).value.find(_.get(moduleID.key).map(id => (id.organization, id.name, id.revision)).contains((asmDep.organization, asmDep.name, asmDep.revision))).get.data) ++
        (products in Compile in packageBin in LocalProject("interactive")).value ++
        (products in Compile in packageBin in LocalProject("scaladoc")).value ++
        (products in Compile in packageBin in LocalProject("repl")).value ++
        (products in Compile in packageBin in LocalProject("repl-jline")).value ++
        (products in Compile in packageBin in LocalProject("repl-jline-embedded")).value,
    includeFilter in unmanagedResources in Compile :=
      "*.tmpl" | "*.xml" | "*.js" | "*.css" | "*.html" | "*.properties" | "*.swf" |
      "*.png" | "*.gif" | "*.gif" | "*.txt",
    // Also include the selected unmanaged resources and source files from the additional projects in the source JAR:
    mappings in Compile in packageSrc ++= {
      val base = (unmanagedResourceDirectories in Compile).value ++
        (unmanagedResourceDirectories in Compile in LocalProject("interactive")).value ++
        (unmanagedResourceDirectories in Compile in LocalProject("scaladoc")).value ++
        (unmanagedResourceDirectories in Compile in LocalProject("repl")).value
      base ** ((includeFilter in unmanagedResources in Compile).value || "*.scala" || "*.psd" || "*.ai" || "*.java") pair Path.relativeTo(base)
    },
    // Include the additional projects in the scaladoc JAR:
    sources in Compile in doc ++= {
      val base =
        (unmanagedSourceDirectories in Compile in LocalProject("interactive")).value ++
        (unmanagedSourceDirectories in Compile in LocalProject("scaladoc")).value ++
        (unmanagedSourceDirectories in Compile in LocalProject("repl")).value
      ((base ** ("*.scala" || "*.java"))
        --- (base ** "Scaladoc*ModelTest.scala") // exclude test classes that depend on partest
      ).get
    },
    scalacOptions in Compile in doc ++= Seq(
      "-doc-root-content", (sourceDirectory in Compile).value + "/rootdoc.txt"
    ),
    Osgi.headers ++= Seq(
      "Import-Package" -> ("jline.*;resolution:=optional," +
                           "org.apache.tools.ant.*;resolution:=optional," +
                           "scala.xml.*;version=\"${range;[====,====];"+versionNumber("scala-xml")+"}\";resolution:=optional," +
                           "scala.*;version=\"${range;[==,=+);${ver}}\"," +
                           "*"),
      "Class-Path" -> "scala-reflect.jar scala-library.jar"
    ),
    // Generate the ScriptEngineFactory service definition. The Ant build does this when building
    // the JAR but sbt has no support for it and it is easier to do as a resource generator:
    generateServiceProviderResources("javax.script.ScriptEngineFactory" -> "scala.tools.nsc.interpreter.Scripted$Factory"),
    managedResourceDirectories in Compile := Seq((resourceManaged in Compile).value),
    fixPom(
      "/project/name" -> <name>Scala Compiler</name>,
      "/project/description" -> <description>Compiler for the Scala Programming Language</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    apiURL := None,
    pomDependencyExclusions ++= List(("org.apache.ant", "ant"), ("org.scala-lang.modules", "scala-asm"))
  )
  .dependsOn(library, reflect)

lazy val interactive = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    name := "scala-compiler-interactive",
    description := "Scala Interactive Compiler"
  )
  .dependsOn(compiler)

lazy val repl = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    connectInput in run := true,
    run := (run in Compile).partialInput(" -usejavacp").evaluated // Automatically add this so that `repl/run` works without additional arguments.
  )
  .dependsOn(compiler, interactive)

lazy val replJline = configureAsSubproject(Project("repl-jline", file(".") / "src" / "repl-jline"))
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    libraryDependencies += jlineDep,
    name := "scala-repl-jline"
  )
  .dependsOn(repl)

lazy val replJlineEmbedded = Project("repl-jline-embedded", file(".") / "target" / "repl-jline-embedded-src-dummy")
  .settings(scalaSubprojectSettings)
  .settings(disablePublishing)
  .settings(
    name := "scala-repl-jline-embedded",
    // There is nothing to compile for this project. Instead we use the compile task to create
    // shaded versions of repl-jline and jline.jar. dist/mkBin puts all of quick/repl,
    // quick/repl-jline and quick/repl-jline-shaded on the classpath for quick/bin scripts.
    // This is different from the Ant build where all parts are combined into quick/repl, but
    // it is cleaner because it avoids circular dependencies.
    compile in Compile := (compile in Compile).dependsOn(Def.task {
      import java.util.jar._
      import collection.JavaConverters._
      val inputs: Iterator[JarJar.Entry] = {
        val repljlineClasses = (products in Compile in replJline).value.flatMap(base => Path.allSubpaths(base).map(x => (base, x._1)))
        val jlineJAR = findJar((dependencyClasspath in Compile).value, jlineDep).get.data
        val jarFile = new JarFile(jlineJAR)
        val jarEntries = jarFile.entries.asScala.filterNot(_.isDirectory).map(entry => JarJar.JarEntryInput(jarFile, entry))
        def compiledClasses = repljlineClasses.iterator.map { case (base, file) => JarJar.FileInput(base, file) }
        (jarEntries ++ compiledClasses).filter(x =>
          x.name.endsWith(".class") || x.name.endsWith(".properties") || x.name.startsWith("META-INF/native") || x.name.startsWith("META-INF/maven")
        )
      }
      import JarJar.JarJarConfig._
      val config: Seq[JarJar.JarJarConfig] = Seq(
        Rule("org.fusesource.**", "scala.tools.fusesource_embedded.@1"),
        Rule("jline.**", "scala.tools.jline_embedded.@1"),
        Rule("scala.tools.nsc.interpreter.jline.**", "scala.tools.nsc.interpreter.jline_embedded.@1"),
        Keep("scala.tools.**")
      )
      val outdir = (classDirectory in Compile).value
      JarJar(inputs, outdir, config)
    }).value,
    connectInput in run := true

  )
  .dependsOn(replJline)

lazy val scaladoc = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    name := "scala-compiler-doc",
    description := "Scala Documentation Generator",
    libraryDependencies ++= Seq(scalaXmlDep),
    includeFilter in unmanagedResources in Compile := "*.html" | "*.css" | "*.gif" | "*.png" | "*.js" | "*.txt" | "*.svg" | "*.eot" | "*.woff" | "*.ttf",
    libraryDependencies ++= ScaladocSettings.webjarResources,
    resourceGenerators in Compile += ScaladocSettings.extractResourcesFromWebjar
  )
  .dependsOn(compiler)

lazy val scalap = configureAsSubproject(project)
  .settings(
    description := "Scala Bytecode Parser",
    // Include decoder.properties
    includeFilter in unmanagedResources in Compile := "*.properties",
    fixPom(
      "/project/name" -> <name>Scalap</name>,
      "/project/description" -> <description>bytecode analysis tool</description>,
      "/project/properties" -> scala.xml.Text("")
    ),
    headerLicense  := Some(HeaderLicense.Custom(
      s"""Scala classfile decoder (${(homepage in ThisBuild).value.get})
         |
         |Copyright EPFL and Lightbend, Inc.
         |
         |Licensed under Apache License 2.0
         |(http://www.apache.org/licenses/LICENSE-2.0).
         |
         |See the NOTICE file distributed with this work for
         |additional information regarding copyright ownership.
         |""".stripMargin)),
    (headerSources in Compile) ~= { xs =>
      val excluded = Set("Memoisable.scala", "Result.scala", "Rule.scala", "Rules.scala", "SeqRule.scala")
      xs filter { x => !excluded(x.getName) }
    },
    (headerResources in Compile) := Nil
  )
  .dependsOn(compiler)

lazy val partestExtras = Project("partest-extras", file(".") / "src" / "partest-extras")
  .dependsOn(replJlineEmbedded, scaladoc)
  .settings(commonSettings)
  .settings(generatePropertiesFileSettings)
  .settings(clearSourceAndResourceDirectories)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    name := "scala-partest-extras",
    description := "Scala Compiler Testing Tool (compiler-specific extras)",
    libraryDependencies += partestDep,
    unmanagedSourceDirectories in Compile := List(baseDirectory.value)
  )

// An instrumented version of BoxesRunTime and ScalaRunTime for partest's "specialized" test category
lazy val specLib = project.in(file("test") / "instrumented")
  .dependsOn(library, reflect, compiler)
  .settings(clearSourceAndResourceDirectories)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    sourceGenerators in Compile += Def.task {
      import scala.collection.JavaConverters._
      val srcBase = (sourceDirectories in Compile in library).value.head / "scala/runtime"
      val targetBase = (sourceManaged in Compile).value / "scala/runtime"
      def patch(srcFile: String, patchFile: String): File = try {
        val patchLines: List[String] = IO.readLines(baseDirectory.value / patchFile)
        val origLines: List[String] = IO.readLines(srcBase / srcFile)
        import difflib.DiffUtils
        val p = DiffUtils.parseUnifiedDiff(patchLines.asJava)
        val r = DiffUtils.patch(origLines.asJava, p)
        val target = targetBase / srcFile
        val patched = r.asScala.toList
        IO.writeLines(target, patched)
        if (patched == origLines) {
          println(p)
          println(patchLines.mkString("\n"))
          println(origLines.mkString("\n"))
          throw new RuntimeException("Patch did not apply any changes! " + baseDirectory.value / patchFile + " / " + (srcBase / srcFile))
        }

        target
      } catch { case ex: Exception =>
        streams.value.log.error(s"Error patching $srcFile: $ex")
        throw ex
      }
      IO.createDirectory(targetBase)
      Seq(
        patch("BoxesRunTime.java", "boxes.patch"),
        patch("ScalaRunTime.scala", "srt.patch")
      )
    }.taskValue
  )

lazy val bench = project.in(file("test") / "benchmarks")
  .dependsOn(library, compiler)
  .settings(instanceSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "test-benchmarks",
    libraryDependencies += "org.openjdk.jol" % "jol-core" % "0.6",
    scalacOptions ++= Seq("-feature", "-opt:l:inline", "-opt-inline-from:**")
  )

lazy val junit = project.in(file("test") / "junit")
  .dependsOn(library, reflect, compiler, partestExtras, scaladoc)
  .settings(clearSourceAndResourceDirectories)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    fork in Test := true,
    javaOptions in Test += "-Xss1M",
    (forkOptions in Test) := (forkOptions in Test).value.withWorkingDirectory((baseDirectory in ThisBuild).value),
    (forkOptions in Test in testOnly) := (forkOptions in Test in testOnly).value.withWorkingDirectory((baseDirectory in ThisBuild).value),
    libraryDependencies ++= Seq(junitDep, junitInterfaceDep, jolDep),
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
    unmanagedSourceDirectories in Compile := Nil,
    unmanagedSourceDirectories in Test := List(baseDirectory.value)
  )

lazy val scalacheck = project.in(file("test") / "scalacheck")
  .dependsOn(library, reflect, compiler, scaladoc)
  .settings(clearSourceAndResourceDirectories)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    // enable forking to workaround https://github.com/sbt/sbt/issues/4009
    fork in Test := true,
    javaOptions in Test += "-Xss1M",
    testOptions ++= {
      if ((fork in Test).value) Nil
      else List(Tests.Cleanup { loader =>
        sbt.internal.inc.ModuleUtilities.getObject("scala.TestCleanup", loader).asInstanceOf[Runnable].run()
      })
    },
    libraryDependencies ++= Seq(scalacheckDep),
    unmanagedSourceDirectories in Compile := Nil,
    unmanagedSourceDirectories in Test := List(baseDirectory.value)
  ).settings(
    // Workaround for https://github.com/sbt/sbt/pull/3985
    List(Keys.test, Keys.testOnly).map(task => parallelExecution in task := false) : _*
  )

lazy val osgiTestFelix = osgiTestProject(
  project.in(file(".") / "target" / "osgiTestFelix"),
  "org.apache.felix" % "org.apache.felix.framework" % "5.0.1")

lazy val osgiTestEclipse = osgiTestProject(
  project.in(file(".") / "target" / "osgiTestEclipse"),
  "org.eclipse.tycho" % "org.eclipse.osgi" % "3.10.100.v20150521-1310")

def osgiTestProject(p: Project, framework: ModuleID) = p
  .dependsOn(library, reflect, compiler)
  .settings(clearSourceAndResourceDirectories)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    fork in Test := true,
    parallelExecution in Test := false,
    libraryDependencies ++= {
      val paxExamVersion = "4.5.0" // Last version which supports Java 6
      Seq(
        junitDep,
        junitInterfaceDep,
        "org.ops4j.pax.exam" % "pax-exam-container-native" % paxExamVersion,
        "org.ops4j.pax.exam" % "pax-exam-junit4" % paxExamVersion,
        "org.ops4j.pax.exam" % "pax-exam-link-assembly" % paxExamVersion,
        "org.ops4j.pax.url" % "pax-url-aether" % "2.4.1",
        "org.ops4j.pax.swissbox" % "pax-swissbox-tracker" % "1.8.1",
        "ch.qos.logback" % "logback-core" % "1.1.3",
        "ch.qos.logback" % "logback-classic" % "1.1.3",
        "org.slf4j" % "slf4j-api" % "1.7.12",
        framework % "test"
      )
    },
    Keys.test in Test := (Keys.test in Test).dependsOn(packageBin in Compile).value,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-q"),
    javaOptions in Test += "-Dscala.bundle.dir=" + (buildDirectory in ThisBuild).value / "osgi",
    (forkOptions in Test in test) := (forkOptions in Test in test).value.withWorkingDirectory((baseDirectory in ThisBuild).value),
    unmanagedSourceDirectories in Test := List((baseDirectory in ThisBuild).value / "test" / "osgi" / "src"),
    unmanagedResourceDirectories in Compile := (unmanagedSourceDirectories in Test).value,
    includeFilter in unmanagedResources in Compile := "*.xml",
    packageBin in Compile := { // Put the bundle JARs required for the tests into build/osgi
      val targetDir = (buildDirectory in ThisBuild).value / "osgi"
      val mappings = ((mkPack in dist).value / "lib").listFiles.collect {
        case f if f.getName.startsWith("scala-") && f.getName.endsWith(".jar") => (f, targetDir / f.getName)
      }
      IO.copy(mappings, CopyOptions() withOverwrite true)
      targetDir
    },
    cleanFiles += (buildDirectory in ThisBuild).value / "osgi"
  )

lazy val partestJavaAgent = Project("partest-javaagent", file(".") / "src" / "partest-javaagent")
  .settings(commonSettings)
  .settings(generatePropertiesFileSettings)
  .settings(disableDocs)
  .settings(
    libraryDependencies += asmDep,
    publishLocal := {},
    publish := {},
    // Setting name to "scala-partest-javaagent" so that the jar file gets that name, which the Runner relies on
    name := "scala-partest-javaagent",
    description := "Scala Compiler Testing Tool (compiler-specific java agent)",
    // add required manifest entry - previously included from file
    packageOptions in (Compile, packageBin) +=
      Package.ManifestAttributes( "Premain-Class" -> "scala.tools.partest.javaagent.ProfilingAgent" ),
    // we need to build this to a JAR
    exportJars := true
  )

lazy val test = project
  .dependsOn(compiler, interactive, replJlineEmbedded, scalap, partestExtras, partestJavaAgent, scaladoc)
  .disablePlugins(plugins.JUnitXmlReportPlugin)
  .configs(IntegrationTest)
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(Defaults.itSettings)
  .settings(
    libraryDependencies ++= Seq(asmDep, partestDep, scalaXmlDep),
    // no main sources
    sources in Compile := Seq.empty,
    // test sources are compiled in partest run, not here
    sources in IntegrationTest := Seq.empty,
    fork in IntegrationTest := true,
    // enable this in 2.13, when tests pass
    //scalacOptions in Compile += "-Yvalidate-pos:parser,typer",
    scalacOptions in Compile -= "-Ywarn-unused:imports",
    javaOptions in IntegrationTest ++= List("-Xmx2G", "-Dpartest.exec.in.process=true", "-Dfile.encoding=UTF-8", "-Duser.language=en", "-Duser.country=US"),
    testOptions in IntegrationTest += Tests.Argument("-Dfile.encoding=UTF-8", "-Duser.language=en", "-Duser.country=US"),
    testFrameworks += new TestFramework("scala.tools.partest.sbt.Framework"),
    testOptions in IntegrationTest += Tests.Argument("-Dpartest.java_opts=-Xmx1024M -Xms64M"),
    testOptions in IntegrationTest += Tests.Argument("-Dpartest.scalac_opts=" + (scalacOptions in Compile).value.mkString(" ")),
    (forkOptions in IntegrationTest) := (forkOptions in IntegrationTest).value.withWorkingDirectory((baseDirectory in ThisBuild).value),
    testOptions in IntegrationTest += {
      val cp = (dependencyClasspath in Test).value
      val baseDir = (baseDirectory in ThisBuild).value
      val instrumentedJar = (packagedArtifact in (LocalProject("specLib"), Compile, packageBin)).value._2
      Tests.Setup { () =>
        // Copy code.jar (resolved in the otherwise unused scope "test") and instrumented.jar (from specLib)to the location where partest expects them
        IO.copyFile(instrumentedJar, baseDir / "test/files/speclib/instrumented.jar")
      }
    },
    definedTests in IntegrationTest += new sbt.TestDefinition(
      "partest",
      // marker fingerprint since there are no test classes
      // to be discovered by sbt:
      new sbt.testing.AnnotatedFingerprint {
        def isModule = true
        def annotationName = "partest"
      }, true, Array()
    ),
    executeTests in IntegrationTest := {
      val log = streams.value.log
      val result = (executeTests in IntegrationTest).value
      val result2 = (executeTests in Test).value
      if (result.overall != TestResult.Error && result.events.isEmpty) {
        // workaround for https://github.com/sbt/sbt/issues/2722
        log.error("No test events found")
        result2.copy(overall = TestResult.Error)
      }
      else result
    },
    testListeners in IntegrationTest += new PartestTestListener(target.value)
  )

lazy val manual = configureAsSubproject(project)
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(
    libraryDependencies ++= Seq(scalaXmlDep, antDep, "org.scala-lang" % "scala-library" % scalaVersion.value),
    classDirectory in Compile := (target in Compile).value / "classes"
  )

lazy val libraryAll = Project("library-all", file(".") / "target" / "library-all-src-dummy")
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(
    name := "scala-library-all",
    publishArtifact in (Compile, packageBin) := false,
    publishArtifact in (Compile, packageSrc) := false,
    libraryDependencies ++= Seq(scalaXmlDep, scalaParserCombinatorsDep, scalaSwingDep),
    apiURL := None,
    fixPom(
      "/project/name" -> <name>Scala Library Powerpack</name>,
      "/project/description" -> <description>The Scala Standard Library and Official Modules</description>
    )
  )
  .dependsOn(library, reflect)

lazy val scalaDist = Project("scala-dist", file(".") / "target" / "scala-dist-dist-src-dummy")
  .settings(commonSettings)
  .settings(disableDocs)
  .settings(
    mappings in Compile in packageBin ++= {
      val binBaseDir = buildDirectory.value / "pack"
      val binMappings = (mkBin in dist).value.pair(Path.relativeTo(binBaseDir), errorIfNone = false)
      // With the way the resource files are spread out over the project sources we can't just add
      // an unmanagedResourceDirectory, so we generate the mappings manually:
      val docBaseDir = (baseDirectory in ThisBuild).value
      val docMappings = (docBaseDir / "doc").allPaths pair Path.relativeTo(docBaseDir)
      val resBaseDir = (baseDirectory in ThisBuild).value / "src/manual/scala/tools/docutil/resources"
      val resMappings = resBaseDir ** ("*.html" | "*.css" | "*.gif" | "*.png") pair (p => Path.relativeTo(resBaseDir)(p).map("doc/tools/" + _))
      docMappings ++ resMappings ++ binMappings
    },
    resourceGenerators in Compile += Def.task {
      val command = "fsc, scala, scalac, scaladoc, scalap"
      val htmlOut = (resourceManaged in Compile).value / "doc/tools"
      val manOut = (resourceManaged in Compile).value / "genman"
      val fixedManOut = (resourceManaged in Compile).value / "man"
      IO.createDirectory(htmlOut)
      IO.createDirectory(manOut / "man1")
      runner.value.run("scala.tools.docutil.ManMaker",
        (fullClasspath in Compile in manual).value.files,
        Seq(command, htmlOut.getAbsolutePath, manOut.getAbsolutePath),
        streams.value.log).failed foreach (sys error _.getMessage)
      (manOut ** "*.1" pair Path.rebase(manOut, fixedManOut)).foreach { case (in, out) =>
        // Generated manpages should always use LF only. There doesn't seem to be a good reason
        // for generating them with the platform EOL first and then converting them but that's
        // what the Ant build does.
        IO.write(out, IO.readBytes(in).filterNot(_ == '\r'))
      }
      (htmlOut ** "*.html").get ++ (fixedManOut ** "*.1").get
    }.taskValue,
    managedResourceDirectories in Compile := Seq((resourceManaged in Compile).value),
    libraryDependencies += jlineDep,
    apiURL := None,
    fixPom(
      "/project/name" -> <name>Scala Distribution Artifacts</name>,
      "/project/description" -> <description>The Artifacts Distributed with Scala</description>,
      "/project/packaging" -> <packaging>jar</packaging>
    ),
    publishArtifact in (Compile, packageSrc) := false
  )
  .dependsOn(libraryAll, compiler, scalap)

lazy val root: Project = (project in file("."))
  .settings(disableDocs)
  .settings(disablePublishing)
  .settings(generateBuildCharacterFileSettings)
  .settings(
    commands ++= ScriptCommands.all,
    extractBuildCharacterPropertiesFile := {
      val jar = (scalaInstance in bootstrap).value.allJars.find(_.getName contains "-compiler").get
      val bc = buildCharacterPropertiesFile.value
      val packagedName = "scala-buildcharacter.properties"
      IO.withTemporaryDirectory { tmp =>
        val extracted = IO.unzip(jar, tmp, new SimpleFilter(_ == packagedName)).headOption.getOrElse {
          throw new RuntimeException(s"No file $packagedName found in bootstrap compiler $jar")
        }
        IO.copyFile(extracted, bc)
        bc
      }
    },
    // Generate (Product|TupleN|Function|AbstractFunction)*.scala files and scaladoc stubs for all AnyVal sources.
    // They should really go into a managedSources dir instead of overwriting sources checked into git but scaladoc
    // source links (could be fixed by shipping these sources with the scaladoc bundles) and scala-js source maps
    // rely on them being on github.
    commands += Command.command("generateSources") { state =>
      val dir = (((baseDirectory in ThisBuild).value) / "src" / "library" / "scala")
      genprod.main(Array(dir.getPath))
      GenerateAnyVals.run(dir.getAbsoluteFile)
      state
    },

    testAll := {
      val results = ScriptCommands.sequence[(Result[Unit], String)](List(
        (Keys.test in Test in junit).result map (_ -> "junit/test"),
        (Keys.test in Test in scalacheck).result map (_ -> "scalacheck/test"),
        (testOnly in IntegrationTest in testP).toTask(" -- run").result map (_ -> "partest run"),
        (testOnly in IntegrationTest in testP).toTask(" -- pos neg jvm").result map (_ -> "partest pos neg jvm"),
        (testOnly in IntegrationTest in testP).toTask(" -- res scalap specialized").result map (_ -> "partest res scalap specialized"),
        (testOnly in IntegrationTest in testP).toTask(" -- instrumented presentation").result map (_ -> "partest instrumented presentation"),
        (testOnly in IntegrationTest in testP).toTask(" -- --srcpath scaladoc").result map (_ -> "partest --srcpath scaladoc"),
        (Keys.test in Test in osgiTestFelix).result map (_ -> "osgiTestFelix/test"),
        (Keys.test in Test in osgiTestEclipse).result map (_ -> "osgiTestEclipse/test"),
        (mimaReportBinaryIssues in library).result map (_ -> "library/mimaReportBinaryIssues"),
        (mimaReportBinaryIssues in reflect).result map (_ -> "reflect/mimaReportBinaryIssues"),
        (compile in Compile in bench).map(_ => ()).result map (_ -> "bench/compile"),
        Def.task(()).dependsOn( // Run these in parallel:
          doc in Compile in library,
          doc in Compile in reflect,
          doc in Compile in compiler,
          doc in Compile in scalap
        ).result map (_ -> "doc")
      )).value
      val log = streams.value.log
      val failed = results.collect { case (Inc(i), d) => (i, d) }
      if (failed.nonEmpty) {
        def showScopedKey(k: Def.ScopedKey[_]): String =
          Vector(
            k.scope.project.toOption.map {
              case p: ProjectRef => p.project
              case p => p
            }.map(_ + "/"),
            k.scope.config.toOption.map(_.name + ":"),
            k.scope.task.toOption.map(_.label + "::")
          ).flatten.mkString + k.key
        val loggedThis, loggedAny = new scala.collection.mutable.HashSet[String]
        def findRootCauses(i: Incomplete, currentTask: String): Vector[(String, Option[Throwable])] = {
          val sk = i.node match {
            case Some(t: Task[_]) =>
              t.info.attributes.entries.collectFirst { case e if e.key == Keys.taskDefinitionKey => e.value.asInstanceOf[Def.ScopedKey[_]] }
                .map(showScopedKey)
            case _ => None
          }
          val task = sk.getOrElse(currentTask)
          val dup = sk.exists(s => !loggedAny.add(s))
          if(sk.exists(s => !loggedThis.add(s))) Vector.empty
          else i.directCause match {
            case Some(e) => Vector((task, if(dup) None else Some(e)))
            case None => i.causes.toVector.flatMap(ch => findRootCauses(ch, task))
          }
        }
        log.error(s"${failed.size} of ${results.length} test tasks failed:")
        failed.foreach { case (i, d) =>
          log.error(s"- $d")
          loggedThis.clear
          findRootCauses(i, "<unkown task>").foreach {
            case (task, Some(ex)) => log.error(s"  - $task failed: $ex")
            case (task, None)     => log.error(s"  - ($task failed)")
          }
        }
        throw new RuntimeException
      }
    },
    setIncOptions
  )
  .aggregate(library, reflect, compiler, compilerOptionsExporter, interactive, repl, replJline, replJlineEmbedded,
    scaladoc, scalap, partestExtras, junit, libraryAll, scalaDist).settings(
    sources in Compile := Seq.empty,
    onLoadMessage := """|*** Welcome to the sbt build definition for Scala! ***
      |Check README.md for more information.""".stripMargin
  )

def setIncOptions = incOptions := {
  incOptions.value
    .withRecompileOnMacroDef(Some(Boolean box false).asJava) // macros in library+reflect are hard-wired to implementations with `FastTrack`.
}

// The following subprojects' binaries are required for building "pack":
lazy val distDependencies = Seq(replJline, replJlineEmbedded, compiler, library, reflect, scalap, scaladoc)

lazy val dist = (project in file("dist"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(scalaSwingDep, jlineDep),
    mkBin := mkBinImpl.value,
    mkQuick := Def.task {
      val cp = (fullClasspath in IntegrationTest in LocalProject("test")).value
      val propsFile = (buildDirectory in ThisBuild).value / "quick" / "partest.properties"
      val props = new java.util.Properties()
      props.setProperty("partest.classpath", cp.map(_.data.getAbsolutePath).mkString(sys.props("path.separator")))
      IO.write(props, null, propsFile)
      (buildDirectory in ThisBuild).value / "quick"
    }.dependsOn((distDependencies.map(products in Runtime in _) :+ mkBin): _*).value,
    mkPack := Def.task { (buildDirectory in ThisBuild).value / "pack" }.dependsOn(packagedArtifact in (Compile, packageBin), mkBin).value,
    target := (baseDirectory in ThisBuild).value / "target" / thisProject.value.id,
    packageBin in Compile := {
      val extraDeps = Set(scalaSwingDep, scalaParserCombinatorsDep, scalaXmlDep)
      val targetDir = (buildDirectory in ThisBuild).value / "pack" / "lib"
      def uniqueModule(m: ModuleID) = (m.organization, m.name.replaceFirst("_.*", ""))
      val extraModules = extraDeps.map(uniqueModule)
      val extraJars = (externalDependencyClasspath in Compile).value.map(a => (a.get(moduleID.key), a.data)).collect {
        case (Some(m), f) if extraModules contains uniqueModule(m) => f
      }
      val jlineJAR = findJar((dependencyClasspath in Compile).value, jlineDep).get.data
      val mappings = extraJars.map(f => (f, targetDir / f.getName)) :+ ((jlineJAR, targetDir / "jline.jar"))
      IO.copy(mappings, CopyOptions() withOverwrite true)
      targetDir
    },
    cleanFiles += (buildDirectory in ThisBuild).value / "quick",
    cleanFiles += (buildDirectory in ThisBuild).value / "pack",
    packagedArtifact in (Compile, packageBin) :=
      (packagedArtifact in (Compile, packageBin))
        .dependsOn(distDependencies.map(packagedArtifact in (Compile, packageBin) in _): _*)
        .value
  )
  .dependsOn(distDependencies.map(p => p: ClasspathDep[ProjectReference]): _*)

/**
 * Configures passed project as a subproject (e.g. compiler or repl)
 * with common settings attached to it.
 *
 * Typical usage is:
 *
 *   lazy val mySubproject = configureAsSubproject(project)
 *
 * We pass `project` as an argument which is in fact a macro call. This macro determines
 * project.id based on the name of the lazy val on the left-hand side.
 */
def configureAsSubproject(project: Project): Project = {
  val base = file(".") / "src" / project.id
  (project in base)
    .settings(scalaSubprojectSettings)
}

lazy val buildDirectory = settingKey[File]("The directory where all build products go. By default ./build")
lazy val mkBin = taskKey[Seq[File]]("Generate shell script (bash or Windows batch).")
lazy val mkQuick = taskKey[File]("Generate a full build, including scripts, in build/quick")
lazy val mkPack = taskKey[File]("Generate a full build, including scripts, in build/pack")
lazy val testAll = taskKey[Unit]("Run all test tasks sequentially")

// Defining these settings is somewhat redundant as we also redefine settings that depend on them.
// However, IntelliJ's project import works better when these are set correctly.
def clearSourceAndResourceDirectories = Seq(Compile, Test).flatMap(config => inConfig(config)(Seq(
  unmanagedSourceDirectories := Nil,
  managedSourceDirectories := Nil,
  unmanagedResourceDirectories := Nil,
  managedResourceDirectories := Nil
)))

lazy val mkBinImpl: Def.Initialize[Task[Seq[File]]] = Def.task {
  import java.io.IOException
  def mkScalaTool(mainCls: String, classpath: Seq[Attributed[File]]): ScalaTool =
    ScalaTool(mainClass  = mainCls,
      classpath  = classpath.toList.map(_.data.getAbsolutePath),
      properties = Map.empty,
      javaOpts   = "-Xmx256M -Xms32M",
      toolFlags  = "")
  val rootDir = (classDirectory in Compile in compiler).value
  val quickOutDir = buildDirectory.value / "quick/bin"
  val packOutDir = buildDirectory.value / "pack/bin"
  def writeScripts(scalaTool: ScalaTool, file: String, outDir: File): Seq[File] = {
    val res = Seq(
      scalaTool.writeScript(file, "unix", rootDir, outDir),
      scalaTool.writeScript(file, "windows", rootDir, outDir)
    )
    res.foreach { f =>
      if(!f.getAbsoluteFile.setExecutable(true, /* ownerOnly: */ false))
        throw new IOException("setExecutable failed")
      if(!f.getAbsoluteFile.setReadable(true, /* ownerOnly: */ false))
        throw new IOException("setReadable failed")
    }
    res
  }

  def mkBin(file: String, mainCls: String, classpath: Seq[Attributed[File]]): Seq[File] =
    writeScripts(mkScalaTool(mainCls, classpath), file, quickOutDir) ++
    writeScripts(mkScalaTool(mainCls, Nil      ), file, packOutDir)

  streams.value.log.info(s"Creating scripts in $quickOutDir and $packOutDir")

  mkBin("scala"    , "scala.tools.nsc.MainGenericRunner", (fullClasspath in Compile in replJlineEmbedded).value) ++
  mkBin("scalac"   , "scala.tools.nsc.Main",              (fullClasspath in Compile in compiler).value) ++
  mkBin("fsc"      , "scala.tools.nsc.CompileClient",     (fullClasspath in Compile in compiler).value) ++
  mkBin("scaladoc" , "scala.tools.nsc.ScalaDoc",          (fullClasspath in Compile in scaladoc).value) ++
  mkBin("scalap"   , "scala.tools.scalap.Main",           (fullClasspath in Compile in scalap).value)
}

/** Generate service provider definition files under META-INF/services */
def generateServiceProviderResources(services: (String, String)*): Setting[_] =
  resourceGenerators in Compile += Def.task {
    services.map { case (k, v) =>
      val f = (resourceManaged in Compile).value / "META-INF/services" / k
      IO.write(f, v + "\n")
      f
    }
  }.taskValue

buildDirectory in ThisBuild := (baseDirectory in ThisBuild).value / "build"

// Add tab completion to partest
commands += Command("partest")(_ => PartestUtil.partestParser((baseDirectory in ThisBuild).value, (baseDirectory in ThisBuild).value / "test")) { (state, parsed) =>
  ("test/it:testOnly -- " + parsed) :: state
}

// Watch the test files also so ~partest triggers on test case changes
watchSources ++= PartestUtil.testFilePaths((baseDirectory in ThisBuild).value, (baseDirectory in ThisBuild).value / "test")

// Add tab completion to scalac et al.
commands ++= {
  val commands =
  List(("scalac",   "compiler", "scala.tools.nsc.Main"),
       ("scala",    "repl-jline-embedded", "scala.tools.nsc.MainGenericRunner"),
       ("scaladoc", "scaladoc", "scala.tools.nsc.ScalaDoc"))

  commands.map {
    case (entryPoint, projectRef, mainClassName) =>
      Command(entryPoint)(_ => ScalaOptionParser.scalaParser(entryPoint, (baseDirectory in ThisBuild).value)) { (state, parsedOptions) =>
        (projectRef + "/runMain " + mainClassName + " -usejavacp " + parsedOptions) :: state
      }
  }
}

addCommandAlias("scalap",   "scalap/compile:runMain              scala.tools.scalap.Main -usejavacp")

lazy val intellij = taskKey[Unit]("Update the library classpaths in the IntelliJ project files.")

def moduleDeps(p: Project, config: Configuration = Compile) = (externalDependencyClasspath in config in p).map(a => (p.id, a.map(_.data)))

// aliases to projects to prevent name clashes
def compilerP = compiler
def testP = test

intellij := {
  import xml._
  import xml.transform._

  val s = streams.value
  val compilerScalaInstance = (scalaInstance in LocalProject("compiler")).value

  val modules: List[(String, Seq[File])] = {
    // for the sbt build module, the dependencies are fetched from the project's build using sbt-buildinfo
    val buildModule = ("scala-build", scalabuild.BuildInfo.buildClasspath.split(java.io.File.pathSeparator).toSeq.map(new File(_)))
    // `sbt projects` lists all modules in the build
    buildModule :: List(
      moduleDeps(bench).value,
      moduleDeps(compilerP).value,
      // moduleDeps(dist).value,                // No sources, therefore no module in IntelliJ
      moduleDeps(interactive).value,
      moduleDeps(junit).value,
      moduleDeps(library).value,
      // moduleDeps(libraryAll).value,          // No sources
      moduleDeps(manual).value,
      moduleDeps(partestExtras).value,
      moduleDeps(partestJavaAgent).value,
      moduleDeps(reflect).value,
      moduleDeps(repl).value,
      moduleDeps(replJline).value,
      // moduleDeps(replJlineEmbedded).value,   // No sources
      // moduleDeps(root).value,                // No sources
      // moduleDeps(scalaDist).value,           // No sources
      moduleDeps(scalacheck, config = Test).value,
      moduleDeps(scaladoc).value,
      moduleDeps(scalap).value,
      moduleDeps(testP).value,
      moduleDeps(compilerOptionsExporter).value
    )
  }

  def moduleDep(name: String, jars: Seq[File]) = {
    val entries = jars.map(f => s"""        <root url="jar://${f.toURI.getRawPath}!/" />""").mkString("\n")
    s"""|    <library name="$name-deps">
        |      <CLASSES>
        |$entries
        |      </CLASSES>
        |      <JAVADOC />
        |      <SOURCES />
        |    </library>""".stripMargin
  }

  def starrDep(jars: Seq[File]) = {
    val entries = jars.map(f => s"""          <root url="file://${f.toURI.getRawPath}" />""").mkString("\n")
    s"""|    <library name="starr" type="Scala">
        |      <properties>
        |        <option name="languageLevel" value="Scala_2_12" />
        |        <compiler-classpath>
        |$entries
        |        </compiler-classpath>
        |      </properties>
        |      <CLASSES />
        |      <JAVADOC />
        |      <SOURCES />
        |    </library>""".stripMargin
  }

  def replaceLibrary(data: Node, libName: String, libType: Option[String], newContent: String) = {
    object rule extends RewriteRule {
      var transformed = false
      def checkAttrs(attrs: MetaData) = {
        def check(key: String, expected: String) = {
          val a = attrs(key)
          a != null && a.text == expected
        }
        check("name", libName) && libType.forall(tp => check("type", tp))
      }

      override def transform(n: Node): Seq[Node] = n match {
        case e @ Elem(_, "library", attrs, _, _, _*) if checkAttrs(attrs) =>
          transformed = true
          XML.loadString(newContent)
        case other =>
          other
      }
    }
    object trans extends RuleTransformer(rule)
    val r = trans(data)
    if (!rule.transformed) sys.error(s"Replacing library classpath for $libName failed, no existing library found.")
    r
  }

  val intellijDir = (baseDirectory in ThisBuild).value / "src/intellij"
  val ipr = intellijDir / "scala.ipr"
  backupIdea(intellijDir)
  if (!ipr.exists) {
    intellijCreateFromSample((baseDirectory in ThisBuild).value)
  }
  s.log.info("Updating library classpaths in src/intellij/scala.ipr.")
  val content = XML.loadFile(ipr)

  val newStarr = replaceLibrary(content, "starr", Some("Scala"), starrDep(compilerScalaInstance.allJars))
  val newModules = modules.foldLeft(newStarr)({
    case (res, (modName, jars)) =>
      if (jars.isEmpty) res // modules without dependencies
      else replaceLibrary(res, s"$modName-deps", None, moduleDep(modName, jars))
  })

  // I can't figure out how to keep the entity escapes for \n in the attribute values after this use of XML transform.
  // Patching the original version back in with more brutish parsing.
  val R = """(?ims)(.*)(<copyright>.*</copyright>)(.*)""".r
  val oldContents = IO.read(ipr)
  XML.save(ipr.getAbsolutePath, newModules)
  oldContents match {
    case R(_, withEscapes, _) =>
      val newContents = IO.read(ipr)
      val R(pre, toReplace, post) = newContents
      IO.write(ipr, pre + withEscapes + post)
    case _ =>
      // .ipr file hasn't been updated from `intellijFromSample` yet
  }
}

lazy val intellijFromSample = taskKey[Unit]("Create fresh IntelliJ project files from src/intellij/*.SAMPLE.")

def backupIdea(ideaDir: File): Unit = {
  val temp = IO.createTemporaryDirectory
  IO.copyDirectory(ideaDir, temp)
  println(s"Backed up existing src/intellij to $temp")
}

intellijFromSample := {
  val s = streams.value
  val intellijDir = (baseDirectory in ThisBuild).value / "src/intellij"
  val ipr = intellijDir / "scala.ipr"
  backupIdea(intellijDir)
  intellijCreateFromSample((baseDirectory in ThisBuild).value)
}

def intellijCreateFromSample(basedir: File): Unit = {
  val files = basedir / "src/intellij" * "*.SAMPLE"
  val copies = files.get.map(f => (f, new File(f.getAbsolutePath.stripSuffix(".SAMPLE"))))
  IO.copy(copies, CopyOptions() withOverwrite true)
}

lazy val intellijToSample = taskKey[Unit]("Update src/intellij/*.SAMPLE using the current IntelliJ project files.")

intellijToSample := {
  val s = streams.value
  val intellijDir = (baseDirectory in ThisBuild).value / "src/intellij"
  val ipr = intellijDir / "scala.ipr"
  backupIdea(intellijDir)
  val existing =intellijDir * "*.SAMPLE"
  IO.delete(existing.get)
  val current = intellijDir * ("*.iml" || "*.ipr")
  val copies = current.get.map(f => (f, new File(f.getAbsolutePath + ".SAMPLE")))
  IO.copy(copies)
}

/** Find a specific module's JAR in a classpath, comparing only organization and name */
def findJar(files: Seq[Attributed[File]], dep: ModuleID): Option[Attributed[File]] = {
  def extract(m: ModuleID) = (m.organization, m.name)
  files.find(_.get(moduleID.key).map(extract _) == Some(extract(dep)))
}

// WhiteSource
whitesourceProduct               := "Lightbend Reactive Platform"
whitesourceAggregateProjectName  := "scala-2.12-stable"
whitesourceIgnoredScopes         := Vector("test", "scala-tool")
