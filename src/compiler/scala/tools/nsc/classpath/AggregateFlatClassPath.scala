/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import java.net.URL
import scala.tools.nsc.util.{ClassRepresentation, ClassPath}

case class AggregateFlatClassPath(aggregates: Seq[FlatClassPath]) extends FlatClassPath {

  override def packages(inPackage: String): Seq[PackageEntry] = {
    val aggregatedPkgs = aggregates.flatMap(_.packages(inPackage)).distinct
    aggregatedPkgs
  }

  override def classes(inPackage: String): Seq[FileEntry] = {
    val aggreagatedClasses = aggregates.flatMap(_.classes(inPackage))
    aggreagatedClasses
  }

  override def list(inPackage: String): (Seq[PackageEntry], Seq[FileEntry]) = {
    val (packages, classes) = aggregates.map(_.list(inPackage)).unzip
    val distinctPackages = packages.flatten.distinct
    val distinctClasses = distinctClassEntries(classes.flatten)
    (distinctPackages, distinctClasses)
  }

  override def findClassFile(className: String): Option[AbstractFile] = findClass(className).flatMap(_.binary)

//  override def findClass(name: String): Option[ClassRepresentation[AbstractFile]] =
  // TODO temporary

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
}
