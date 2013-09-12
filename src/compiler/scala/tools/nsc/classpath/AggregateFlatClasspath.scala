package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile

case class AggregateFlatClasspath(aggregates: Seq[FlatClasspath]) extends FlatClasspath {
  def packages(inPackage: String): Seq[PackageEntry] = {
    val aggregatedPkgs = aggregates.map(_.packages(inPackage)).flatten.distinct
    aggregatedPkgs
  }
  def classes(inPackage: String): Seq[ClassfileEntry] = {
    val aggreagatedClasses = aggregates.map(_.classes(inPackage)).flatten
    aggreagatedClasses
  }
  private def distinctClassEntries(classEntries: Seq[ClassfileEntry]): Seq[ClassfileEntry] = {
    val collectedClassNames = collection.mutable.Set.empty[String]
    val collectedEntries = collection.mutable.ArrayBuffer.empty[ClassfileEntry]
    classEntries foreach { classEntry =>
      val className = classEntry.name
      if (!collectedClassNames.contains(className)) {
        collectedClassNames += className
        collectedEntries += classEntry
      }
    }
    collectedEntries
  }
  def list(inPackage: String): (Seq[PackageEntry], Seq[ClassfileEntry]) = {
    val (packages, classes) = aggregates.map(_.list(inPackage)).unzip
    val distinctPackages = packages.flatten.distinct
    val distinctClasses = distinctClassEntries(classes.flatten)
    (distinctPackages, distinctClasses)
  }
  def findClassFile(className: String): Option[AbstractFile] = {
    val lastIndex = className.lastIndexOf('.')
    val (pkg, simpleClassName) = if (lastIndex == -1) (FlatClasspath.RootPackage, className) else {
      (className.substring(0, lastIndex), className.substring(lastIndex+1))
    }
    classes(pkg).find(_.name == simpleClassName).map(_.file)
  }
}
