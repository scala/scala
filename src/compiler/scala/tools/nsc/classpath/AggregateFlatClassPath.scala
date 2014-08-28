/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.net.URL
import scala.annotation.tailrec
import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.util.ClassRepresentation

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
    val distinctClassesAndSources = mergeClassesAndSources(classesAndSources: _*)
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

  override def findClass(className: String): Option[ClassRepresentation[AbstractFile]] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)

    @tailrec
    def findClassEntry(aggregates: Seq[FlatClassPath]): Option[ClassFileEntry] =
      if (aggregates.nonEmpty) {
        val classEntry = aggregates.head.classes(pkg)
          .find(_.name == simpleClassName)
        if (classEntry.isDefined) classEntry
        else findClassEntry(aggregates.tail)
      } else None

    @tailrec
    def findSourceEntry(aggregates: Seq[FlatClassPath]): Option[SourceFileEntry] =
      if (aggregates.nonEmpty) {
        val sourceEntry = aggregates.head.sources(pkg)
          .find(_.name == simpleClassName)
        if (sourceEntry.isDefined) sourceEntry
        else findSourceEntry(aggregates.tail)
      } else None

    val classEntry = findClassEntry(aggregates)
    val sourceEntry = findSourceEntry(aggregates)

    mergeClassesAndSources(classEntry.toList, sourceEntry.toList).headOption
  }

  override def asURLs: Seq[URL] = aggregates.flatMap(_.asURLs)

  override def asClassPathStrings: Seq[String] = aggregates.map(_.asClassPathString).distinct

  override def asSourcePathString: String = ClassPath.join(aggregates map (_.asSourcePathString): _*)

  /**
   * Returns only one entry for each name. If there's both souce and class entry,
   * it creates entry containing both of them.
   */
  private def mergeClassesAndSources(entries: Seq[ClassRepClassPathEntry]*): Seq[ClassRepClassPathEntry] = {
    // based on implementation from MergedClassPath
    import scala.collection.mutable._
    var count = 0
    val indices = HashMap[String, Int]()
    val cls = new ArrayBuffer[ClassRepClassPathEntry](1024)

    for {
      entriesPart <- entries
      entry <- entriesPart
    } {
      val name = entry.name
      if (indices contains name) {
        val idx = indices(name)
        val existing = cls(idx)

        if (existing.binary.isEmpty && entry.binary.isDefined)
          cls(idx) = ClassAndSourceFilesEntry(entry.binary.get, existing.source.get)
        if (existing.source.isEmpty && entry.source.isDefined)
          cls(idx) = ClassAndSourceFilesEntry(existing.binary.get, entry.source.get)
      }
      else {
        indices(name) = count
        cls += entry
        count += 1
      }
    }
    cls.toIndexedSeq
  }
}
