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

package scala.tools.nsc
package doc
package model

import java.nio.file.Paths

import base._
import scala.tools.nsc.io.AbstractFile

/** This trait extracts all required information for documentation from compilation units */
trait MemberLookup extends base.MemberLookupBase {
  thisFactory: ModelFactory =>

  import global._
  import definitions.{ NothingClass, AnyClass, AnyValClass, AnyRefClass }

  override def internalLink(sym: Symbol, site: Symbol): Option[LinkTo] =
    findTemplateMaybe(sym) match {
      case Some(tpl) => Some(LinkToTpl(tpl))
      case None =>
        findTemplateMaybe(site) flatMap { inTpl =>
          inTpl.members find (_.asInstanceOf[EntityImpl].sym == sym) map (LinkToMember(_, inTpl))
        }
    }

  override def chooseLink(links: List[LinkTo]): LinkTo = {
    val mbrs = links.collect {
      case lm@LinkToMember(mbr: MemberEntity, _) => (mbr, lm)
    }
    if (mbrs.isEmpty)
      links.head
    else
      mbrs.min(Ordering[MemberEntity].on[(MemberEntity, LinkTo)](_._1))._2
  }

  override def toString(link: LinkTo) = link match {
    case LinkToTpl(tpl: EntityImpl) => tpl.sym.toString
    case LinkToMember(mbr: EntityImpl, inTpl: EntityImpl) =>
      mbr.sym.signatureString + " in " + inTpl.sym.toString
    case _ => link.toString
  }

  override def findExternalLink(sym: Symbol, name: String): Option[LinkTo] = {
    val sym1 =
      if (sym == AnyClass || sym == AnyRefClass || sym == AnyValClass || sym == NothingClass)
        definitions.ScalaPackageClass.info.member(newTermName("package"))
      else if (sym.hasPackageFlag)
        /* Get package object which has associatedFile ne null */
        sym.info.member(newTermName("package"))
      else sym
    classpathEntryFor(sym1) flatMap { path =>
      if (isJDK(sym1)) {
        Some(LinkToExternalTpl(name, jdkUrl(path, sym1), makeTemplate(sym)))
      }
      else {
        settings.extUrlMapping get path map { url =>
          LinkToExternalTpl(name, url, makeTemplate(sym))
        }
      }
    }
  }

  private def classpathEntryFor(s: Symbol): Option[String] = {
    Option(s.associatedFile).flatMap(_.underlyingSource).map { src =>
      val path = src.canonicalPath
      if(path.endsWith(".class")) { // Individual class file -> Classpath entry is root dir
        val nesting = s.ownerChain.count(_.hasPackageFlag)
        if(nesting > 0) {
          val p = 0.until(nesting).foldLeft(src) {
            case (null, _) => null
            case (f, _) => f.container
          }
          if(p eq null) path else p.canonicalPath
        } else path
      } else path // JAR file (and fallback option)
    }
  }

  /**
   * Check if this file is a child of the given directory string. Can only be used
   * on directories that actually exist in the file system.
   */
  def isChildOf(f: AbstractFile, dir: String): Boolean = {
    val parent = Paths.get(dir).toAbsolutePath().toString
    f.canonicalPath.startsWith(parent)
  }

  private def isJDK(sym: Symbol) =
    sym.associatedFile.underlyingSource.map(f => isChildOf(f, (sys.props("java.home")))).getOrElse(false)

  // ISSUE-12820
  import scala.util.Try
  private lazy val classGetModule = Try {
    val clazz = Class.forName("java.lang.Class")
    clazz.getMethod("getModule")
  }.toOption
  private lazy val moduleGetName = Try {
    val clazz = Class.forName("java.lang.Module")
    clazz.getMethod("getName")
  }.toOption

  def jdkUrl(path: String, sym: Symbol): String = {
    if (path.endsWith(".jmod") && javaVersion >= 11) {
      val tokens = path.split(java.io.File.separatorChar)
      val module = tokens.last.stripSuffix(".jmod")
      s"$jdkUrl/$module"
    } else if (path.endsWith("ct.sym") && javaVersion >= 11) {
      (for {
        clazz <- Try(Class.forName(sym.javaClassName)).toOption
        getModule <- classGetModule
        module <- Try(getModule.invoke(clazz)).toOption
        getModuleName <- moduleGetName
        moduleName <-
          Try(getModuleName.invoke(module)).toOption
            .map(Option(_))
            .flatten
      } yield {
        s"$jdkUrl/$moduleName"
      }).getOrElse(jdkUrl)
    }
    else {
      jdkUrl
    }
  }

  def jdkUrl: String = {
    if (settings.jdkApiDocBase.isDefault)
      defaultJdkUrl
    else
      settings.jdkApiDocBase.value
  }

  lazy val defaultJdkUrl = {
    if (javaVersion < 11) {
      s"https://docs.oracle.com/javase/$javaVersion/docs/api"
    }
    else {
      s"https://docs.oracle.com/en/java/javase/$javaVersion/docs/api"
    }
  }

  lazy val javaVersion: Int =
    global.settings.releaseValue
      .getOrElse(scala.util.Properties.javaSpecVersion)
      .split('.')
      .take(2)
      .map(_.toIntOption) match {
      case Array(Some(1), Some(n)) => n // example: 1.8.0_242
      case Array(Some(n))          => n // example: 14
      case Array(Some(n), _)       => n // example: 11.0.7
      case _                       => 8 // shrug!
    }

  override def warnNoLink = !settings.docNoLinkWarnings.value
}
