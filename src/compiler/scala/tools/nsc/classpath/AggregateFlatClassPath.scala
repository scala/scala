package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile

case class AggregateFlatClassPath(aggregates: Seq[FlatClassPath]) extends FlatClassPath {
  def packages(inPackage: String): Seq[PackageEntry] = {
    val aggregatedPkgs = aggregates.map(_.packages(inPackage)).flatten.distinct
    aggregatedPkgs
  }
  def classes(inPackage: String): Seq[ClassFileEntry] = {
    val aggreagatedClasses = aggregates.map(_.classes(inPackage)).flatten
    aggreagatedClasses
  }
  private def distinctClassEntries(classEntries: Seq[ClassFileEntry]): Seq[ClassFileEntry] = {
    val collectedClassNames = collection.mutable.Set.empty[String]
    val collectedEntries = collection.mutable.ArrayBuffer.empty[ClassFileEntry]
    classEntries foreach { classEntry =>
      val className = classEntry.name
      if (!collectedClassNames.contains(className)) {
        collectedClassNames += className
        collectedEntries += classEntry
      }
    }
    collectedEntries
  }
  def list(inPackage: String): (Seq[PackageEntry], Seq[ClassFileEntry]) = {
    val (packages, classes) = aggregates.map(_.list(inPackage)).unzip
    val distinctPackages = packages.flatten.distinct
    val distinctClasses = distinctClassEntries(classes.flatten)
    (distinctPackages, distinctClasses)
  }
  def findClassFile(className: String): Option[AbstractFile] = {
    val lastIndex = className.lastIndexOf('.')
    val (pkg, simpleClassName) = if (lastIndex == -1) (FlatClassPath.RootPackage, className) else {
      (className.substring(0, lastIndex), className.substring(lastIndex+1))
    }
    classes(pkg).find(_.name == simpleClassName).map(_.file)
  }
}
