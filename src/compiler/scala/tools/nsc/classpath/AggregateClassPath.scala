/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.classpath

import java.net.URL

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
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    aggregatesForPackage(PackageName(pkg)).iterator.map(_.findClassFile(className)).collectFirst {
      case Some(x) => x
    }
  }
  private[this] val packageIndex: collection.mutable.Map[String, Seq[ClassPath]] = collection.mutable.Map()
  private def aggregatesForPackage(pkg: PackageName): Seq[ClassPath] = packageIndex.synchronized {
    packageIndex.getOrElseUpdate(pkg.dottedString, aggregates.filter(_.hasPackage(pkg)))
  }

  // This method is performance sensitive as it is used by SBT's ExtractDependencies phase.
  override def findClass(className: String): Option[ClassRepresentation] = {
    val pkg = PackageNameUtils.pkgName(className)

    //manually unrolled for performance
    var result = Option.empty[ClassRepresentation]
    var done = false
    val it = aggregatesForPackage(PackageName(pkg)).iterator.flatMap(_.findClass(className))
    while (it.hasNext && !done) {
      val next = it.next()
      if (result.isEmpty) {
        result = Some(next)
        done = next.source.isDefined && next.binary.isDefined
      } else {
        val res = result.get
        if (res.source.isEmpty && next.source.isDefined) {
          result = Some(ClassAndSourceFilesEntry(res.binary.get, next.source.get))
          done = true
        } else if (res.binary.isEmpty && next.binary.isDefined) {
          result = Some(ClassAndSourceFilesEntry(next.binary.get, res.source.get))
          done = true
        }
      }
    }
    result
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
    val packages = new java.util.HashSet[PackageEntry]()
    val classesAndSources = new java.util.HashMap[String, ClassRepresentation]()
    val onPackage: PackageEntry => Unit = packages.add(_)
    /**
     * Only one entry for each name. Merges existing and new entry, but doesnt overwrite source or binary of an existing entry
     */
    val onClassesAndSources: ClassRepresentation => Unit = { entry =>
      val existing = classesAndSources.putIfAbsent(entry.name, entry)
      if (existing ne null) {
        val existingBinary = existing.binary
        val existingSource = existing.source
        
        var entryBinary: Option[AbstractFile] = null
        if (existingBinary.isEmpty && { entryBinary = entry.binary; entryBinary.isDefined})
          classesAndSources.put(entry.name, ClassAndSourceFilesEntry(entryBinary.get, existingSource.get))
        else {

          var entrySource: Option[AbstractFile] = null
          if (existingSource.isEmpty && { entrySource = entry.source; entrySource.isDefined})
            classesAndSources.put(entry.name, ClassAndSourceFilesEntry(existingBinary.get, entrySource.get))
        }
      }
    }

    aggregates.foreach { cp =>
      try {
        cp match {
          case ecp: EfficientClassPath =>
            ecp.list(inPackage, onPackage, onClassesAndSources)
          case _ =>
            val entries = cp.list(inPackage)
            entries._1 foreach onPackage
            entries._2 foreach onClassesAndSources
        }
      } catch {
        case ex: java.io.IOException =>
          val e = FatalError(ex.getMessage)
          e.initCause(ex)
          throw e
      }
    }

    ClassPathEntries(toSeq(packages), toSeq(classesAndSources.values))
  }
  private def toSeq[T <: AnyRef: Manifest](data: java.util.Collection[T]): Seq[T] = if (data isEmpty) Nil else data.toArray(new Array[T](data.size()))

  private def getDistinctEntries[EntryType <: ClassRepresentation](getEntries: ClassPath => Seq[EntryType]): Seq[EntryType] = {
    val seenNames = collection.mutable.HashSet[String]()
    val entriesBuffer = new ArrayBuffer[EntryType](1024)
    for {
      cp <- aggregates
      entry <- getEntries(cp)
    } {
      if (seenNames.add(entry.name)) entriesBuffer += entry
    }
    if (entriesBuffer isEmpty) Nil else entriesBuffer.toIndexedSeq
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
