/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.symtab;

public interface Modifiers {

    // modifiers
    int DEFERRED      = 0x00000001;   // was `abstract' for members
    int FINAL         = 0x00000002;
    int PRIVATE       = 0x00000004;
    int PROTECTED     = 0x00000008;

    int SEALED        = 0x00000010;
    int OVERRIDE      = 0x00000020;
    int CASE          = 0x00000040;
    int ABSTRACT      = 0x00000080;   // abstract class, or used in conjunction
                                      // with abstract override.
                                      // Note difference to DEFERRED!

    int DEF           = 0x00000100;   // a def parameter
    int REPEATED      = 0x00000200;   // a repeated parameter
    int SYNTHETIC     = 0x00000400;
    int DEPRECATED    = 0x00000800;

    int JAVA          = 0x00001000;   // symbol was defined by a Java class
    int MODUL         = 0x00002000;   // symbol is module or class implementing a module
    int MUTABLE       = 0x00004000;   // symbol is a mutable variable.
    int VIEWBOUND     = 0x00004000;   // type symbol has a <% bound.
    int PARAM         = 0x00008000;   // symbol is a (type) parameter to a method

    int INITIALIZED   = 0x00010000;   // symbol's definition is complete
    int LOCKED        = 0x00020000;   // temporary flag to catch cyclic dependencies
    int ACCESSED      = 0x00040000;   // symbol was accessed at least once
    int SELECTOR      = 0x00080000;   // symbol was used as selector in Select

    int PACKAGE       = 0x00100000;   // symbol is a java package.
    int STABLE        = 0x00800000;   // functions that are assumed to be stable
                                    // (typically, access methods for valdefs)

    int CAPTURED      = 0x01000000;   // variables is accessed from
                                      // nested function. Set by LambdaLift
    int INCONSTRUCTOR = 0x01000000;   // transient flag for Analyzer
    int CASEACCESSOR  = 0x02000000;   // function is a case constructor

    int ACCESSOR      = 0x04000000;   // function is an access function for a
                                      // value or variable
    int BRIDGE        = 0x08000000;    // function is a bridge method.
    int LIFTED        = BRIDGE;        // transient flag for lambdalift
    int ALTERNATIVE   = BRIDGE;        // transient flag for pickle/unpickle
    int SNDTIME       = BRIDGE;        // debug

    int INTERFACE     = 0x10000000;   // symbol is a Java interface
    int TRAIT         = 0x20000000;   // symbol is a Trait

    int COVARIANT     = 0x40000000;   // symbol is a covariant type variable
    int CONTRAVARIANT = 0x80000000;   // symbol is a contravariant type variable

    // masks
    int SOURCEFLAGS   = 0x00000077 | DEF | REPEATED | MODUL | MUTABLE | PACKAGE | PARAM | TRAIT | COVARIANT | CONTRAVARIANT;  // these modifiers can be set in source programs.
    int ACCESSFLAGS   = PRIVATE | PROTECTED;
    int VARIANCES     = COVARIANT | CONTRAVARIANT;
    int CONSTRFLAGS   = CASE | JAVA;
    /** Module-class flags inherited by their module */
    int CLASS2MODULEFLAGS = ACCESSFLAGS | DEPRECATED | JAVA | PACKAGE;

    public static class Helper {

        public static boolean isAbstract(int flags) {
	    // todo: ABSTRACT and DEFERRED should be separated.
            return (flags & DEFERRED) != 0 ||
		(flags & (ABSTRACT | OVERRIDE)) == ABSTRACT;
        }

        public static boolean isFinal(int flags) {
            return (flags & FINAL)     != 0;
        }

        public static boolean isPrivate(int flags) {
            return (flags & PRIVATE)   != 0;
        }

        public static boolean isProtected(int flags) {
            return (flags & PROTECTED) != 0;
        }

        public static boolean isSealed(int flags) {
            return (flags & SEALED) != 0;
        }

        public static boolean isOverride(int flags) {
            return (flags & OVERRIDE)  != 0;
        }

        public static boolean isCase(int flags) {
            return (flags & CASE)      != 0;
        }

        public static boolean isCaseAccessor(int flags) {
            return (flags & CASEACCESSOR) != 0;
        }

        public static boolean isInterface(int flags) {
            return (flags & INTERFACE) != 0;
        }

        public static boolean isDef(int flags) {
            return (flags & DEF)       != 0;
        }

        public static boolean isModClass(int flags) {
            return (flags & MODUL)  != 0;
        }

        public static boolean isJava(int flags) {
            return (flags & JAVA)  != 0;
        }

        public static boolean isNoVal(int flags) {
            return (flags & PACKAGE)  != 0;
        }

        public static String toString(int flags) {
            StringBuffer buffer = new StringBuffer();
            toString(buffer, flags);
            return buffer.toString();
        }

        public static void toString(StringBuffer buffer, int flags) {
            //buffer.append(flags).append(": ");//debug
            int marker = buffer.length();
            if (isPrivate(flags)) buffer.append("private ");
            if (isProtected(flags)) buffer.append("protected ");
            if (isAbstract(flags)) buffer.append("abstract ");
            if (isFinal(flags)) buffer.append("final ");
            if (isSealed(flags)) buffer.append("qualified ");
            if (isInterface(flags)) buffer.append("interface ");
            if (isCase(flags)) buffer.append("case ");
            if (isDef(flags)) buffer.append("def ");
            if (isOverride(flags)) buffer.append("override ");
            int length = buffer.length();
            buffer.setLength(length - (length == marker ? 0 : 1));
        }
    }
}
