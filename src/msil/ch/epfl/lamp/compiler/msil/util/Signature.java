/*
 * System.Reflection-like API for acces to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil.util;

import ch.epfl.lamp.compiler.msil.Type;

/**
 * Signatures
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public interface Signature {

    //##########################################################################

    /** Marks end of a list. */
    public static final int  ELEMENT_TYPE_END = 0x00;
    /** void */
    public static final int  ELEMENT_TYPE_VOID = 0x01;
    /** boolean */
    public static final int  ELEMENT_TYPE_BOOLEAN = 0x02;
    /** char */
    public static final int  ELEMENT_TYPE_CHAR = 0x03;
    /** signed byte */
    public static final int  ELEMENT_TYPE_I1 = 0x04;
    /** byte */
    public static final int  ELEMENT_TYPE_U1 = 0x05;
    /** short */
    public static final int  ELEMENT_TYPE_I2 = 0x06;
    /** unsigned short */
    public static final int  ELEMENT_TYPE_U2 = 0x07;
    /** int */
    public static final int  ELEMENT_TYPE_I4 = 0x08;
    /** unsigned int */
    public static final int  ELEMENT_TYPE_U4 = 0x09;
    /** long */
    public static final int  ELEMENT_TYPE_I8 = 0x0a;
    /** unsigned long */
    public static final int  ELEMENT_TYPE_U8 = 0x0b;
    /** float */
    public static final int  ELEMENT_TYPE_R4 = 0x0c;
    /** double */
    public static final int  ELEMENT_TYPE_R8 = 0x0d;
    /** string */
    public static final int  ELEMENT_TYPE_STRING = 0x0e;
    /** Followed by <type> token. */
    public static final int  ELEMENT_TYPE_PTR = 0x0f;
    /** Followed by <type> token. */
    public static final int  ELEMENT_TYPE_BYREF = 0x10;
    /** Followed by <type> token */
    public static final int  ELEMENT_TYPE_VALUETYPE = 0x11;
    /** Followed by <type> token */
    public static final int  ELEMENT_TYPE_CLASS = 0x12;
    /** <type> <rank> <boundsCount> <bound1> ... <loCount> <lo1> ... */
    public static final int  ELEMENT_TYPE_ARRAY = 0x14;
    /***/
    public static final int  ELEMENT_TYPE_TYPEDBYREF = 0x16;
    /** System.IntPtr */
    public static final int  ELEMENT_TYPE_I = 0x18;
    /** System.UIntPtr */
    public static final int  ELEMENT_TYPE_U = 0x19;
    /** Followed by full method signature. */
    public static final int  ELEMENT_TYPE_FNPTR = 0x1b;
    /** System.Object. */
    public static final int  ELEMENT_TYPE_OBJECT = 0x1c;
    /** Single-dim array with 0 lower bound. */
    public static final int  ELEMENT_TYPE_SZARRAY = 0x1d;
    /** Required modifier : followed by a TypeDef or TypeRef token. */
    public static final int  ELEMENT_TYPE_CMOD_REQD = 0x1f;
    /** Optional modifier : followed by a TypeDef or TypeRef token. */
    public static final int  ELEMENT_TYPE_CMOD_OPT = 0x20;
    /** Implemented within the CLI. */
    public static final int  ELEMENT_TYPE_INTERNAL = 0x21;
    /** Or'd with following element types. */
    public static final int  ELEMENT_TYPE_MODIFIER = 0x40;
    /** Sentinel for varargs method signature. */
    public static final int  ELEMENT_TYPE_SENTINEL = 0x41;
    /**Denotes a local variable that points at a pinned object. */
    public static final int  ELEMENT_TYPE_PINNED = 0x45;

    //##########################################################################
    // signature designators

    public static final int HASTHIS = 0x20;
    public static final int EXPLICITTHIS = 0x40;
    public static final int DEFAULT = 0x00;
    public static final int VARARG = 0x05;
    public static final int SENTINEL = 0x41;
    public static final int C = 0x01;
    public static final int STDCALL = 0x02;
    public static final int THISCALL = 0x03;
    public static final int FASTCALL = 0x04;
    public static final int FIELD = 0x06;
    public static final int PROPERTY = 0x08;
    public static final int LOCAL_SIG = 0x07;

    //##########################################################################
    // extra IDs used in the serialization format of named arguments
    // to custom attributes. Reverse-engineered from compiled C# example

    /** What follows is a string with the full name of the type. */
    public static final int X_ELEMENT_TYPE_TYPE = 0x50;

    /** What follows is a string with the full name of the enumeration type*/
    public static final int X_ELEMENT_TYPE_ENUM = 0x55;

    /** The named argument specifies a field. */
    public static final int X_ELEMENT_KIND_FIELD = 0x53;

    /** The named argument specifies a property. */
    public static final int X_ELEMENT_KIND_PROPERTY = 0x54;

    //##########################################################################
}  // interface Signature
