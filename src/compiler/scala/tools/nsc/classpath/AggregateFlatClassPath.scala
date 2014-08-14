/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.net.URL
import scala.annotation.tailrec
import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.ClassPath

case class AggregateFlatClassPath(aggregates: Seq[FlatClassPath]) extends FlatClassPath {
  override def packages(inPackage: String): Seq[PackageEntry] = {
    val aggregatedPkgs = aggregates.flatMap(_.packages(inPackage)).distinct
    aggregatedPkgs
  }

  override def classes(inPackage: String): Seq[ClassFileEntry] = {
    val aggreagatedClasses = aggregates.flatMap(_.classes(inPackage))
    aggreagatedClasses
  }

  override def sources(inPackage: String): Seq[SourceFileEntry] = {
    val aggreagatedSources = aggregates.flatMap(_.sources(inPackage))
    aggreagatedSources
  }

  override def list(inPackage: String): FlatClassPathEntries = {
    val (packages, classesAndSources) = aggregates.map(_.list(inPackage)).unzip
    val distinctPackages = packages.flatten.distinct
    // FIXME do merge like in the case of old classpath!
    // mergedClassesAndSources
    val distinctClassesAndSources = distinctClassEntries(classesAndSources.flatten)
    FlatClassPathEntries(distinctPackages, distinctClassesAndSources)
  }

  override def findClassFile(className: String): Option[AbstractFile] = {
    @tailrec
    def find(aggregates: Seq[FlatClassPath]): Option[AbstractFile] =
      if (aggregates.nonEmpty) {
        val classFile = aggregates.head.findClassFile(className)
        if (classFile.isDefined) classFile
        else find(aggregates.tail)
      } else None

    find(aggregates)
  }

  // FIXME create better implementation - something like:
  //  override def findClass(className: String): Option[ClassRepresentation[AbstractFile]] = {
  //    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
  //
  //    mergedClassesAndSources(pkg).find(_.name == simpleClassName)
  //  }

  override def asURLs: Seq[URL] = aggregates.flatMap(_.asURLs)

  override def asClassPathStrings: Seq[String] = aggregates.map(_.asClassPathString).distinct

  override def asSourcePathString: String = ClassPath.join(aggregates map (_.asSourcePathString): _*)

  private def distinctClassEntries(classEntries: Seq[FileEntry]): Seq[FileEntry] = {
    val collectedClassNames = collection.mutable.Set.empty[String]
    val collectedEntries = collection.mutable.ArrayBuffer.empty[FileEntry]
    classEntries foreach { classEntry =>
      val className = classEntry.name
      if (!collectedClassNames.contains(className)) {
        collectedClassNames += className
        collectedEntries += classEntry
      }
    }
    collectedEntries
  }

  private def mergedClassesAndSources(inPackage: String): Seq[FileEntry] = ???
}

