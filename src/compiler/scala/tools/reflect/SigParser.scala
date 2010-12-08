/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package reflect

import java.lang.reflect.GenericSignatureFormatError

/** The usual reflection song and dance to avoid referencing
 *  any sun.* classes.
 */
class SigParser {
  val SunSignatureParser = "sun.reflect.generics.parser.SignatureParser"
  lazy val makeMethod = Class.forName(SunSignatureParser) getMethod "make"
  def make() = makeMethod.invoke(null).asInstanceOf[SignatureParserInterface]

  private def wrap(op: => Any) =
    try   { op ; true }
    catch { case _: GenericSignatureFormatError => false }

  def verifyClass(s: String)  = wrap(make() parseClassSig s)
  def verifyMethod(s: String) = wrap(make() parseMethodSig s)
  def verifyType(s: String)   = wrap(make() parseTypeSig s)

  type ClassSignature <: AnyRef
  type MethodTypeSignature <: AnyRef
  type TypeSignature <: AnyRef

  type SignatureParserInterface = {
    def parseClassSig(s: String): ClassSignature
    def parseMethodSig(s: String): MethodTypeSignature
    def parseTypeSig(s: String): TypeSignature
  }
}
object SigParser extends SigParser { }
