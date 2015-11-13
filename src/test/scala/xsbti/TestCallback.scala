package xsbti

import java.io.File
import scala.collection.mutable.ArrayBuffer
import xsbti.api.SourceAPI
import xsbti.api.DependencyContext
import xsbti.api.DependencyContext._

class TestCallback(override val nameHashing: Boolean = false) extends AnalysisCallback {
  val sourceDependencies = new ArrayBuffer[(File, File, DependencyContext)]
  val binaryDependencies = new ArrayBuffer[(File, String, File, DependencyContext)]
  val products = new ArrayBuffer[(File, File, String)]
  val usedNames = scala.collection.mutable.Map.empty[File, Set[String]].withDefaultValue(Set.empty)
  val apis: scala.collection.mutable.Map[File, SourceAPI] = scala.collection.mutable.Map.empty

  def sourceDependency(dependsOn: File, source: File, inherited: Boolean): Unit = {
    val context = if (inherited) DependencyByInheritance else DependencyByMemberRef
    sourceDependency(dependsOn, source, context)
  }
  def sourceDependency(dependsOn: File, source: File, context: DependencyContext): Unit = {
    sourceDependencies += ((dependsOn, source, context))
    ()
  }
  def binaryDependency(binary: File, name: String, source: File, inherited: Boolean): Unit = {
    val context = if (inherited) DependencyByInheritance else DependencyByMemberRef
    binaryDependency(binary, name, source, context)
  }
  def binaryDependency(binary: File, name: String, source: File, context: DependencyContext): Unit = {
    binaryDependencies += ((binary, name, source, context))
    ()
  }
  def generatedClass(source: File, module: File, name: String): Unit = {
    products += ((source, module, name))
    ()
  }

  def usedName(source: File, name: String): Unit = { usedNames(source) += name }
  def api(source: File, sourceAPI: SourceAPI): Unit = {
    assert(!apis.contains(source), s"The `api` method should be called once per source file: $source")
    apis(source) = sourceAPI
  }
  def problem(category: String, pos: xsbti.Position, message: String, severity: xsbti.Severity, reported: Boolean): Unit = ()
}
