/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
**  $Id$
*/

package scala.tools.scalap;


/** Encoding of modifiers for Scala definitions.
 *
 *  @author		Matthias Zenger
 *	@version	1.0, 10/02/2004
 */
object Flags {

    final val DEFERRED     = 0x00000001; // non-concrete definition
    final val FINAL        = 0x00000002; // final definition
    final val PRIVATE      = 0x00000004; // private definition
    final val PROTECTED    = 0x00000008; // protected definition
    final val SEALED       = 0x00000010; // sealed class
    final val OVERRIDE     = 0x00000020; // definition overrides other definition
    final val CASE         = 0x00000040; // case class
    final val ABSTRACT     = 0x00000080; // abstract class

    final val DEF          = 0x00000100; // a def parameter
    final val REPEATED     = 0x00000200; // a repeated parameter

    final val SYNTHETIC    = 0x00000400; // a synthetic definition
    final val DEPRECATED   = 0x00000800; // a deprecated definition

    final val JAVA         = 0x00001000; // defined by a Java class
    final val OBJECT       = 0x00002000; // a singleton object
    final val MUTABLE      = 0x00004000; // a mutable variable
    final val PARAM        = 0x00008000; // a (type) parameter of a method
    final val PACKAGE      = 0x00100000; // a java package

    final val CASEACCESSOR = 0x02000000; // a case constructor
    final val ACCESSOR     = 0x04000000; // an access function for a value/variable
    final val BRIDGE       = 0x08000000; // a bridge method.

    final val INTERFACE    = 0x10000000; // a Java interface
    final val TRAIT        = 0x20000000; // a trait

    final val COVAR        = 0x40000000; // a covariant type variable
    final val CONTRAVAR    = 0x80000000; // a contravariant type variable

	final val TF_STAR      = 4;			 // a repeated type
	final val TF_DEF       = 8;          // a 'def' type

	/** Check if a flag is present in the given flag set.
	 */
	def is(flag: Int, flagset: Int): Boolean = (flag & flagset) != 0;

    /** Convert a set of modifiers into a readable string.
     */
    def toString(flags: Int): String =  {
        val buffer = new StringBuffer();
        if (is(PRIVATE, flags))
            buffer.append("private ");
        if (is(PROTECTED, flags))
            buffer.append("protected ");
        if (is(ABSTRACT, flags) && !is(TRAIT, flags))
            buffer.append("abstract ");
        if (is(FINAL, flags) && !is(OBJECT, flags))
            buffer.append("final ");
        if (is(SEALED, flags))
            buffer.append("sealed ");
        if (is(CASE, flags))
            buffer.append("case ");
        if (is(DEF, flags))
            buffer.append("def ");
        if (is(OVERRIDE, flags))
            buffer.append("override ");
        buffer.toString()
    }
}
