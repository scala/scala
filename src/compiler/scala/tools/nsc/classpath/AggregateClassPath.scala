/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import java.net.URL
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.FatalError
import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.util.ClassRepresentation

/**
 * A classpath unifying multiple class- and sourcepath entries.
 * The Classpath can obtain entries for classes and sources independently
 * so it tries to do operations quite optimally - iterating only these collections
 * which are needed in the given moment and only as far as it's necessary.
 *
 * @param aggregates classpath instances containing entries which this class processes
 */
case class AggregateClassPath(aggregates: Seq[ClassPath]) extends ClassPath {
  override def findClassFile(className: String): Option[AbstractFile] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    aggregatesForPackage(pkg).iterator.map(_.findClassFile(className)).collectFirst {
      case Some(x) => x
    }
  }
  private[this] val packageIndex: collection.mutable.Map[String, Seq[ClassPath]] = collection.mutable.Map()
  private def aggregatesForPackage(pkg: String): Seq[ClassPath] = packageIndex.synchronized {
    packageIndex.getOrElseUpdate(pkg, aggregates.filter(_.hasPackage(pkg)))
  }

  // This method is performance sensitive as it is used by SBT's ExtractDependencies phase.
  override def findClass(className: String): Option[ClassRepresentation] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)

    def findEntry(isSource: Boolean): Option[ClassRepresentation] = {
      aggregatesForPackage(pkg).iterator.map(_.findClass(className)).collectFirst {
        case Some(s: SourceFileEntry) if isSource => s
        case Some(s: ClassFileEntry) if !isSource => s
      }
    }

    val classEntry = findEntry(isSource = false)
    val sourceEntry = findEntry(isSource = true)

    (classEntry, sourceEntry) match {
      case (Some(c: ClassFileEntry), Some(s: SourceFileEntry)) => Some(ClassAndSourceFilesEntry(c.file, s.file))
      case (c @ Some(_), _) => c
      case (_, s) => s
    }
  }

  override def asURLs: Seq[URL] = aggregates.flatMap(_.asURLs)

  override def asClassPathStrings: Seq[String] = aggregates.map(_.asClassPathString).distinct

  override def asSourcePathString: String = ClassPath.join(aggregates map (_.asSourcePathString): _*)

  override private[nsc] def packages(inPackage: String): Seq[PackageEntry] = {
    val aggregatedPackages = aggregates.flatMap(_.packages(inPackage)).distinct
    aggregatedPackages
  }

  override private[nsc] def classes(inPackage: String): Seq[ClassFileEntry] =
    getDistinctEntries(_.classes(inPackage))

  override private[nsc] def sources(inPackage: String): Seq[SourceFileEntry] =
    getDistinctEntries(_.sources(inPackage))

  override private[nsc] def hasPackage(pkg: String) = aggregates.exists(_.hasPackage(pkg))
  override private[nsc] def list(inPackage: String): ClassPathEntries = {
    val (packages, classesAndSources) = aggregates.map { cp =>
      try {
        cp.list(inPackage)
      } catch {
        case ex: java.io.IOException =>
          val e = FatalError(ex.getMessage)
          e.initCause(ex)
          throw e
      }
    }.unzip
    val distinctPackages = packages.flatten.distinct
    val distinctClassesAndSources = mergeClassesAndSources(classesAndSources: _*)
    ClassPathEntries(distinctPackages, distinctClassesAndSources)
  }

  /**
   * Returns only one entry for each name. If there's both a source and a class entry, it
   * creates an entry containing both of them. If there would be more than one class or source
   * entries for the same class it always would use the first entry of each type found on a classpath.
   */
  private def mergeClassesAndSources(entries: Seq[ClassRepresentation]*): Seq[ClassRepresentation] = {
    // based on the implementation from MergedClassPath
    var count = 0
    val indices = collection.mutable.HashMap[String, Int]()
    val mergedEntries = new ArrayBuffer[ClassRepresentation](1024)

    for {
      partOfEntries <- entries
      entry <- partOfEntries
    } {
      val name = entry.name
      if (indices contains name) {
        val index = indices(name)
        val existing = mergedEntries(index)

        if (existing.binary.isEmpty && entry.binary.isDefined)
          mergedEntries(index) = ClassAndSourceFilesEntry(entry.binary.get, existing.source.get)
        if (existing.source.isEmpty && entry.source.isDefined)
          mergedEntries(index) = ClassAndSourceFilesEntry(existing.binary.get, entry.source.get)
      }
      else {
        indices(name) = count
        mergedEntries += entry
        count += 1
      }
    }
    mergedEntries.toIndexedSeq
  }

  private def getDistinctEntries[EntryType <: ClassRepresentation](getEntries: ClassPath => Seq[EntryType]): Seq[EntryType] = {
    val seenNames = collection.mutable.HashSet[String]()
    val entriesBuffer = new ArrayBuffer[EntryType](1024)
    for {
      cp <- aggregates
      entry <- getEntries(cp) if !seenNames.contains(entry.name)
    } {
      entriesBuffer += entry
      seenNames += entry.name
    }
    entriesBuffer.toIndexedSeq
  }
}

object AggregateClassPath {
  def createAggregate(parts: ClassPath*): ClassPath = {
    val elems = new ArrayBuffer[ClassPath]()
    parts foreach {
      case AggregateClassPath(ps) => elems ++= ps
      case p => elems += p
    }
    if (elems.size == 1) elems.head
    else AggregateClassPath(elems.toIndexedSeq)
  }
}
