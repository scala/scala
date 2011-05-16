/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.reflect.internal.util

class Statistics extends StatBase {
  val singletonBaseTypeSeqCount = new Counter
  val compoundBaseTypeSeqCount = new Counter
  val typerefBaseTypeSeqCount = new Counter
  val findMemberCount = new Counter
  val noMemberCount = new Counter
  val multMemberCount = new Counter
  val findMemberNanos = new Timer
  val asSeenFromCount = new Counter
  val asSeenFromNanos = new Timer
  val subtypeCount = new Counter
  val subtypeNanos = new Timer
  val sametypeCount = new Counter
  val rawTypeCount = new Counter
  val rawTypeFailed = new SubCounter(rawTypeCount)
  val findMemberFailed = new SubCounter(findMemberCount)
  val subtypeFailed = new SubCounter(subtypeCount)
  val rawTypeImpl = new SubCounter(rawTypeCount)
  val findMemberImpl = new SubCounter(findMemberCount)
  val subtypeImpl = new SubCounter(subtypeCount)
  val baseTypeSeqCount = new Counter
  val baseTypeSeqLenTotal = new Counter
  val typeSymbolCount = new Counter
  val classSymbolCount = new Counter
}

object Statistics extends Statistics

