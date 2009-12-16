/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \  Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/  (c) 2003-2010, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/      http://scala-lang.org/
**
*/

// $Id$

package scala.tools.scalap

import scala.tools.scalap.scalax.rules.scalasig._
import scala.tools.nsc.util.ScalaClassLoader.getSystemLoader

/** Temporary decoder.  This would be better off in the scala.tools.nsc
 *  but right now the compiler won't acknowledge scala.tools.scalap
 *  when it's bootstrapping, so the reference has to go from here to there.
 */
object Decode {
  private def getAliasSymbol(t: Type): Symbol = t match {
    case TypeRefType(_, s, _)   => s
    case PolyType(typeRef, _)   => getAliasSymbol(typeRef)
    case _                      => NoSymbol
  }

  /** Returns a map of Alias -> Type for the given package.
   */
  def typeAliases(pkg: String) = {
    for {
      clazz <- getSystemLoader.tryToLoadClass[AnyRef](pkg + ".package")
      ssig <- ScalaSigParser.parse(clazz)
    }
    yield {
      val typeAliases = ssig.symbols partialMap { case x: AliasSymbol => x }
      Map(typeAliases map (x => (x.name, getAliasSymbol(x.infoType).path)): _*)
    }
  }
}

