/*
 * Scala classfile decoder (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.scalap
package scalax
package rules
package scalasig

import ScalaSigEntryParsers._

trait Symbol extends Flags {
  def name: String
  def parent: Option[Symbol]
  def children: Seq[Symbol]

  def path: String = parent.map(_.path + ".").getOrElse("") + name
}

case object NoSymbol extends Symbol {
  def name = "<no symbol>"
  def parent = None
  def hasFlag(flag: Long) = false
  def children = Nil
}

abstract class ScalaSigSymbol extends Symbol {
  def applyRule[A](rule: EntryParser[A]): A = expect(rule)(entry)
  def applyScalaSigRule[A](rule: ScalaSigParsers.Parser[A]) = ScalaSigParsers.expect(rule)(entry.scalaSig)

  def entry: ScalaSig#Entry
  def index = entry.index

  lazy val children: Seq[Symbol] = applyScalaSigRule(ScalaSigParsers.symbols) filter (sym => sym.parent == Some(this) && !sym.isParam)
  lazy val attributes: Seq[AttributeInfo] = applyScalaSigRule(ScalaSigParsers.attributes) filter (_.symbol == this)
}

case class ExternalSymbol(name: String, parent: Option[Symbol], entry: ScalaSig#Entry) extends ScalaSigSymbol {
  override def toString = path
  def hasFlag(flag: Long) = false
}

case class SymbolInfo(name: String, owner: Symbol, flags: Int, privateWithin: Option[AnyRef], info: Int, entry: ScalaSig#Entry) {
  def symbolString(any: AnyRef) = any match {
    case sym: SymbolInfoSymbol => sym.index.toString
    case other => other.toString
  }

  override def toString = name + ", owner=" + symbolString(owner) + ", flags=" + flags.toHexString + ", info=" + info + (privateWithin match {
    case Some(any) => ", privateWithin=" + symbolString(any)
    case None => " "
  })
}

abstract class SymbolInfoSymbol extends ScalaSigSymbol {
  def symbolInfo: SymbolInfo

  def entry = symbolInfo.entry
  def name = symbolInfo.name
  def parent = Some(symbolInfo.owner)
  def hasFlag(flag: Long) = (symbolInfo.flags & flag) != 0L

  lazy val infoType = applyRule(parseEntry(typeEntry)(symbolInfo.info))
}

case class TypeSymbol(symbolInfo: SymbolInfo) extends SymbolInfoSymbol{
  override def path = name
}

case class AliasSymbol(symbolInfo: SymbolInfo) extends SymbolInfoSymbol{
  override def path = name
}
case class ClassSymbol(symbolInfo: SymbolInfo, thisTypeRef: Option[Int]) extends SymbolInfoSymbol {
  lazy val selfType = thisTypeRef.map{(x: Int) => applyRule(parseEntry(typeEntry)(x))}
}
case class ObjectSymbol(symbolInfo: SymbolInfo) extends SymbolInfoSymbol
case class MethodSymbol(symbolInfo: SymbolInfo, aliasRef: Option[Int]) extends SymbolInfoSymbol
