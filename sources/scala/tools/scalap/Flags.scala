/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
**  $Id$
*/

package scalap;


object Flags {

    final val DEFERRED     = 0x00000001;
    final val FINAL        = 0x00000002;
    final val PRIVATE      = 0x00000004;
    final val PROTECTED    = 0x00000008;

    final val SEALED       = 0x00000010;
    final val OVERRIDE     = 0x00000020;
    final val CASE         = 0x00000040;
    final val ABSTRACT     = 0x00000080; // abstract class

    final val DEF          = 0x00000100; // a def parameter
    final val REPEATED     = 0x00000200; // a repeated parameter
    final val SYNTHETIC    = 0x00000400;
    final val DEPRECATED   = 0x00000800;

    final val JAVA         = 0x00001000; // symbol was defined by a Java class
    final val OBJECT       = 0x00002000; // symbol is module or class implementing a module
    final val MUTABLE      = 0x00004000; // symbol is a mutable variable.
    final val PARAM        = 0x00008000; // symbol is a (type) parameter to a method

    final val INITIALIZED  = 0x00010000; // symbol's definition is complete
    final val LOCKED       = 0x00020000; // temporary flag to catch cyclic dependencies
    final val ACCESSED     = 0x00040000; // symbol was accessed at least once
    final val SELECTOR     = 0x00080000; // symbol was used as selector in Select

    final val PACKAGE      = 0x00100000; // symbol is a java package.
    final val LABEL        = 0x00200000; // symbol is a label symbol
    final val STATIC       = 0x00400000; // "static" inner classes (i.e. after class norm.)
    final val STABLE       = 0x00800000; // functions that are assumed to be stable
                                         // (typically, access methods for valdefs)

    final val CAPTURED     = 0x01000000; // variables is accessed from nested function.
    final val CASEACCESSOR = 0x02000000; // function is a case constructor

    final val ACCESSOR     = 0x04000000; // function is an access function for a
                                         // value or variable
    final val BRIDGE       = 0x08000000; // function is a bridge method.

    final val INTERFACE    = 0x10000000; // symbol is a Java interface
    final val TRAIT        = 0x20000000; // symbol is a Trait

    final val COVAR        = 0x40000000; // symbol is a covariant type variable
    final val CONTRAVAR    = 0x80000000; // symbol is a contravariant type variable

    // masks
    final val SOURCEFLAGS   = 0x00000077
                            | DEF | REPEATED | OBJECT | MUTABLE
                            | PACKAGE | PARAM | TRAIT | COVAR
                            | CONTRAVAR;  // these modifiers can be set in source programs
    final val ACCESSFLAGS   = PRIVATE | PROTECTED;
    final val VARIANCES     = COVAR | CONTRAVAR;

    def isDeferred(flags: Int): Boolean = (flags & DEFERRED) != 0;

    def isAbstract(flags: Int): Boolean = (flags & ABSTRACT) != 0;

    def isFinal(flags: Int): Boolean = (flags & FINAL) != 0;

    def isPrivate(flags: Int): Boolean = (flags & PRIVATE)   != 0;

    def isProtected(flags: Int): Boolean = (flags & PROTECTED) != 0;

    def isSealed(flags: Int): Boolean = (flags & SEALED) != 0;

    def isOverride(flags: Int): Boolean = (flags & OVERRIDE) != 0;

    def isCase(flags: Int): Boolean = (flags & CASE) != 0;

    def isCaseAccessor(flags: Int): Boolean = (flags & CASEACCESSOR) != 0;

    def isInterface(flags: Int): Boolean = (flags & INTERFACE) != 0;

    def isTrait(flags: Int): Boolean = (flags & TRAIT) != 0;

    def isObj(flags: Int): Boolean = (flags & OBJECT) != 0;

    def isDef(flags: Int): Boolean = (flags & DEF) != 0;

    def isObjClass(flags: Int): Boolean = (flags & OBJECT) != 0;

    def isStatic(flags: Int): Boolean = (flags & STATIC) != 0;

    def isJava(flags: Int): Boolean = (flags & JAVA) != 0;

    def isNoVal(flags: Int): Boolean = (flags & PACKAGE) != 0;

    def toString(flags: Int): String = {
        val buffer = new StringBuffer();
        var x: StringBuffer = buffer;
        if (isPrivate(flags))
            x = buffer.append("private ");
        if (isProtected(flags))
            x = buffer.append("protected ");
        if (isAbstract(flags) && !isTrait(flags))
            x = buffer.append("abstract ");
        if (isFinal(flags) && !isObj(flags))
            x = buffer.append("final ");
        if (isSealed(flags))
            x = buffer.append("sealed ");
        if (isCase(flags))
            x = buffer.append("case ");
        if (isDef(flags))
            x = buffer.append("def ");
        if (isOverride(flags))
            x = buffer.append("override ");
        buffer.toString()
    }
}
