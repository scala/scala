package scala.tools.xsbt

import xsbti.api.{ClassLike, DependencyContext}
import xsbti.{Action, AnalysisCallback2, DiagnosticCode, DiagnosticRelatedInformation, Position, Severity, UseScope, VirtualFile, VirtualFileRef}

import java.io.File
import java.nio.file.Path
import java.util
import java.util.Optional
import scala.collection.mutable.ArrayBuffer

class TestCallback extends AnalysisCallback2 {
  case class TestUsedName(name: String, scopes: util.EnumSet[UseScope])

  val classDependencies = new ArrayBuffer[(String, String, DependencyContext)]
  val binaryDependencies =
    new ArrayBuffer[(Path, String, String, DependencyContext)]
  val productClassesToSources =
    scala.collection.mutable.Map.empty[Path, VirtualFileRef]
  val usedNamesAndScopes =
    scala.collection.mutable.Map.empty[String, Set[TestUsedName]].withDefaultValue(Set.empty)
  val classNames =
    scala.collection.mutable.Map
      .empty[VirtualFileRef, Set[(String, String)]]
      .withDefaultValue(Set.empty)
  val apis: scala.collection.mutable.Map[VirtualFileRef, Set[ClassLike]] =
    scala.collection.mutable.Map.empty

  def usedNames = usedNamesAndScopes.view.mapValues(_.map(_.name)).toMap

  override def startSource(source: File): Unit = ???
  override def startSource(source: VirtualFile): Unit = {
    assert(
      !apis.contains(source),
      s"The startSource can be called only once per source file: $source"
    )
    apis(source) = Set.empty
  }

  def classDependency(
                       onClassName: String,
                       sourceClassName: String,
                       context: DependencyContext
                     ): Unit = {
    if (onClassName != sourceClassName)
      classDependencies += ((onClassName, sourceClassName, context))
    ()
  }

  override def binaryDependency(
                                 classFile: File,
                                 onBinaryClassName: String,
                                 fromClassName: String,
                                 fromSourceFile: File,
                                 context: DependencyContext
                               ): Unit = ???

  override def binaryDependency(
                                 onBinary: Path,
                                 onBinaryClassName: String,
                                 fromClassName: String,
                                 fromSourceFile: VirtualFileRef,
                                 context: DependencyContext
                               ): Unit = {
    binaryDependencies += ((onBinary, onBinaryClassName, fromClassName, context))
    ()
  }

  override def generatedNonLocalClass(
                                       sourceFile: File,
                                       classFile: File,
                                       binaryClassName: String,
                                       srcClassName: String
                                     ): Unit = ???

  override def generatedNonLocalClass(
                                       sourceFile: VirtualFileRef,
                                       classFile: Path,
                                       binaryClassName: String,
                                       srcClassName: String
                                     ): Unit = {
    productClassesToSources += ((classFile, sourceFile))
    classNames(sourceFile) += ((srcClassName, binaryClassName))
    ()
  }

  override def generatedLocalClass(
                                    sourceFile: File,
                                    classFile: File
                                  ): Unit = ???

  override def generatedLocalClass(
                                    sourceFile: VirtualFileRef,
                                    classFile: Path
                                  ): Unit = {
    productClassesToSources += ((classFile, sourceFile))
    ()
  }

  def usedName(className: String, name: String, scopes: util.EnumSet[UseScope]): Unit =
    usedNamesAndScopes(className) += TestUsedName(name, scopes)

  override def api(source: File, api: ClassLike): Unit = ???

  override def api(source: VirtualFileRef, api: ClassLike): Unit = {
    apis(source) += api
    ()
  }

  override def mainClass(source: File, className: String): Unit = ()

  override def mainClass(source: VirtualFileRef, className: String): Unit = ()

  override def enabled(): Boolean = true

  def problem(
               category: String,
               pos: xsbti.Position,
               message: String,
               severity: xsbti.Severity,
               reported: Boolean
             ): Unit = ()

  override def problem2(what: String,
                        pos: Position,
                        msg: String,
                        severity: Severity,
                        reported: Boolean,
                        rendered: Optional[String],
                        diagnosticCode: Optional[DiagnosticCode],
                        diagnosticRelatedInformation: util.List[DiagnosticRelatedInformation],
                        actions: util.List[Action]): Unit = ()

  override def dependencyPhaseCompleted(): Unit = {}

  override def apiPhaseCompleted(): Unit = {}

  override def classesInOutputJar(): util.Set[String] = java.util.Collections.emptySet()

  override def isPickleJava: Boolean = false

  override def getPickleJarPair = Optional.empty()
}

object TestCallback {
  case class ExtractedClassDependencies(
                                         memberRef: Map[String, Set[String]],
                                         inheritance: Map[String, Set[String]],
                                         localInheritance: Map[String, Set[String]]
                                       )
  object ExtractedClassDependencies {
    def fromPairs(
                   memberRefPairs: Seq[(String, String)],
                   inheritancePairs: Seq[(String, String)],
                   localInheritancePairs: Seq[(String, String)]
                 ): ExtractedClassDependencies = {
      ExtractedClassDependencies(
        pairsToMultiMap(memberRefPairs),
        pairsToMultiMap(inheritancePairs),
        pairsToMultiMap(localInheritancePairs)
      )
    }

    private def pairsToMultiMap[A, B](pairs: Seq[(A, B)]): Map[A, Set[B]] = {
      import scala.collection.{mutable => m}
      val emptyMultiMap = new m.HashMap[A, m.Set[B]]
      val multiMap = pairs.foldLeft(emptyMultiMap) {
        case (acc, (key, value)) => acc.get(key) match {
          case None =>
            val s = m.Set.empty[B]
            s += value
            acc(key) = s
            acc
          case Some(s) =>
            s += value
            acc
        }
      }
      // convert all collections to immutable variants
      multiMap.toMap.view.mapValues(_.toSet).toMap.withDefaultValue(Set.empty)
    }
  }
}
