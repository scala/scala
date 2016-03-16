package xsbti

import java.io.File
import xsbti.api.{ DependencyContext, ClassLike }

import scala.collection.mutable.ArrayBuffer

class TestCallback(override val nameHashing: Boolean = false) extends AnalysisCallback {
  val classDependencies = new ArrayBuffer[(String, String, DependencyContext)]
  val binaryDependencies = new ArrayBuffer[(File, String, String, DependencyContext)]
  val products = new ArrayBuffer[(File, File)]
  val usedNames = scala.collection.mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty)
  val declaredClasses = scala.collection.mutable.Map.empty[File, Set[String]].withDefaultValue(Set.empty)
  val classNames = scala.collection.mutable.Map.empty[File, Set[(String, String)]].withDefaultValue(Set.empty)
  val apis: scala.collection.mutable.Map[File, Set[ClassLike]] = scala.collection.mutable.Map.empty

  def startSource(source: File): Unit = {
    assert(!apis.contains(source), s"The startSource can be called only once per source file: $source")
    apis(source) = Set.empty
  }

  def classDependency(onClassName: String, sourceClassName: String, context: DependencyContext): Unit = {
    if (onClassName != sourceClassName)
      classDependencies += ((onClassName, sourceClassName, context))
    ()
  }
  def binaryDependency(onBinary: File, onBinaryClassName: String, fromClassName: String, fromSourceFile: File, context: DependencyContext): Unit = {
    binaryDependencies += ((onBinary, onBinaryClassName, fromClassName, context))
    ()
  }
  def generatedNonLocalClass(source: File, module: File, binaryClassName: String, srcClassName: String): Unit = {
    products += ((source, module))
    classNames(source) += ((srcClassName, binaryClassName))
    ()
  }

  def generatedLocalClass(source: File, module: File): Unit = {
    products += ((source, module))
    ()
  }

  def usedName(className: String, name: String): Unit = { usedNames(className) += name }
  override def declaredClass(sourceFile: File, className: String): Unit =
    declaredClasses(sourceFile) += className

  def api(source: File, api: ClassLike): Unit = {
    apis(source) += api
    ()
  }
  def problem(category: String, pos: xsbti.Position, message: String, severity: xsbti.Severity, reported: Boolean): Unit = ()
}

object TestCallback {
  case class ExtractedClassDependencies(memberRef: Map[String, Set[String]], inheritance: Map[String, Set[String]],
    localInheritance: Map[String, Set[String]])
  object ExtractedClassDependencies {
    def fromPairs(
      memberRefPairs: Seq[(String, String)],
      inheritancePairs: Seq[(String, String)],
      localInheritancePairs: Seq[(String, String)]
    ): ExtractedClassDependencies = {
      ExtractedClassDependencies(pairsToMultiMap(memberRefPairs), pairsToMultiMap(inheritancePairs),
        pairsToMultiMap(localInheritancePairs))
    }

    private def pairsToMultiMap[A, B](pairs: Seq[(A, B)]): Map[A, Set[B]] = {
      import scala.collection.mutable.{ HashMap, MultiMap }
      val emptyMultiMap = new HashMap[A, scala.collection.mutable.Set[B]] with MultiMap[A, B]
      val multiMap = pairs.foldLeft(emptyMultiMap) {
        case (acc, (key, value)) =>
          acc.addBinding(key, value)
      }
      // convert all collections to immutable variants
      multiMap.toMap.mapValues(_.toSet).withDefaultValue(Set.empty)
    }
  }
}
