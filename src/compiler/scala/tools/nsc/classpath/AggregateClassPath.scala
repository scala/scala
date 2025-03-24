/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.classpath

import java.net.URL

import scala.collection.immutable.ArraySeq.unsafeWrapArray
import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.FatalError
import scala.reflect.io.AbstractFile
import scala.tools.nsc.util.{ClassPath, ClassRepresentation, EfficientClassPath}

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
    val (pkg, _) = PackageNameUtils.separatePkgAndClassNames(className)
    aggregatesForPackage(PackageName(pkg)).iterator.map(_.findClassFile(className)).collectFirst {
      case Some(x) => x
    }
  }
  private[this] val packageIndex: collection.mutable.Map[String, Seq[ClassPath]] = collection.mutable.Map()
  private def aggregatesForPackage(pkg: PackageName): Seq[ClassPath] = packageIndex.synchronized {
    packageIndex.getOrElseUpdate(pkg.dottedString, aggregates.filter(_.hasPackage(pkg)))
  }

  // This method is performance sensitive as it is used by sbt's ExtractDependencies phase.
  override def findClass(className: String): Option[ClassRepresentation] = {
    // workaround a performance bug in exiting versions of Zinc.
    // https://github.com/sbt/zinc/issues/757
    // Retire this code once the Zinc releases a fix and it is widely used.
    val noByteCode = className match {
      case s if s.indexOf('<') >= 0 => true
      case "scala.Nothing" => true
      case "scala.Singleton" => true
      case "scala.Null" => true
      case _ => false
    }
    if (noByteCode) None
    else {
      val (pkg, _) = PackageNameUtils.separatePkgAndClassNames(className)
      val packageName = PackageName(pkg)

      def findEntry(isSource: Boolean): Option[ClassRepresentation] = {
        aggregatesForPackage(packageName).iterator.map(_.findClass(className)).collectFirst {
          case Some(s: SourceFileEntry) if isSource => s
          case Some(s: ClassFileEntry) if !isSource => s
        }
      }

      val classEntry  = findEntry(isSource = false)
      val sourceEntry = findEntry(isSource = true)

      (classEntry, sourceEntry) match {
        case (Some(c: ClassFileEntry), Some(s: SourceFileEntry)) => Some(ClassAndSourceFilesEntry(c.file, s.file))
        case (c@Some(_), _)                                      => c
        case (_, s)                                              => s
      }
    }
  }

  override def asURLs: Seq[URL] = aggregates.flatMap(_.asURLs)

  override def asClassPathStrings: Seq[String] = aggregates.map(_.asClassPathString).distinct

  override def asSourcePathString: String = ClassPath.join(aggregates map (_.asSourcePathString): _*)

  override private[nsc] def packages(inPackage: PackageName): Seq[PackageEntry] = {
    val aggregatedPackages = aggregates.flatMap(_.packages(inPackage)).distinct
    aggregatedPackages
  }

  override private[nsc] def classes(inPackage: PackageName): Seq[ClassFileEntry] =
    getDistinctEntries(_.classes(inPackage))

  override private[nsc] def sources(inPackage: PackageName): Seq[SourceFileEntry] =
    getDistinctEntries(_.sources(inPackage))

  override private[nsc] def hasPackage(pkg: PackageName) = aggregates.exists(_.hasPackage(pkg))
  override private[nsc] def list(inPackage: PackageName): ClassPathEntries = {
    val packages: java.util.HashSet[PackageEntry] = new java.util.HashSet[PackageEntry]()
    val classesAndSourcesBuffer = collection.mutable.ArrayBuffer[ClassRepresentation]()
    val onPackage: PackageEntry => Unit = packages.add(_)
    val onClassesAndSources: ClassRepresentation => Unit = classesAndSourcesBuffer += _

    aggregates.foreach { cp =>
      try {
        cp match {
          case ecp: EfficientClassPath =>
            ecp.list(inPackage, onPackage, onClassesAndSources)
          case _ =>
            val entries = cp.list(inPackage)
            entries._1.foreach(entry => packages.add(entry))
            classesAndSourcesBuffer ++= entries._2
        }
      } catch {
        case ex: java.io.IOException =>
          val e = FatalError(ex.getMessage)
          e.initCause(ex)
          throw e
      }
    }

    val distinctPackages: Seq[PackageEntry] = if (packages == null) Nil else unsafeWrapArray(packages.toArray(new Array[PackageEntry](packages.size())))
    val distinctClassesAndSources = mergeClassesAndSources(classesAndSourcesBuffer)
    ClassPathEntries(distinctPackages, distinctClassesAndSources)
  }

  /**
   * Returns only one entry for each name. If there's both a source and a class entry, it
   * creates an entry containing both of them. If there would be more than one class or source
   * entries for the same class it always would use the first entry of each type found on a classpath.
   */
  private def mergeClassesAndSources(entries: scala.collection.Seq[ClassRepresentation]): Seq[ClassRepresentation] = {
    var count = 0
    val indices = new java.util.HashMap[String, Int]((entries.size * 1.25).toInt)
    val mergedEntries = new ArrayBuffer[ClassRepresentation](entries.size)
    for {
      entry <- entries
    } {
      val name = entry.name
      if (indices.containsKey(name)) {
        val index = indices.get(name)
        val existing = mergedEntries(index)

        if (existing.binary.isEmpty && entry.binary.isDefined)
          mergedEntries(index) = ClassAndSourceFilesEntry(entry.binary.get, existing.source.get)
        if (existing.source.isEmpty && entry.source.isDefined)
          mergedEntries(index) = ClassAndSourceFilesEntry(existing.binary.get, entry.source.get)
      }
      else {
        indices.put(name, count)
        mergedEntries += entry
        count += 1
      }
    }
    if (mergedEntries.isEmpty) Nil else mergedEntries.toIndexedSeq
  }

  private def getDistinctEntries[EntryType <: ClassRepresentation](getEntries: ClassPath => Seq[EntryType]): Seq[EntryType] = {
    val seenNames = collection.mutable.HashSet[String]()
    val entriesBuffer = new ArrayBuffer[EntryType](1024)
    for {
      cp <- aggregates
      entry <- getEntries(cp)
    } {
      if (seenNames.add(entry.name)) entriesBuffer += entry
    }
    if (entriesBuffer.isEmpty) Nil else entriesBuffer.toIndexedSeq
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
