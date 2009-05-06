/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;

/**
 * Specifies flags that control binding and the way in which
 * the search for members and types is conducted by reflection.
 *
 * Note: You must specify Instance or Static along with Public or NonPublic
 * or no members will be returned.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class BindingFlags {

    //##########################################################################

    // disallows extending the class;
    private BindingFlags() {}

    /**
     * Specifies no binding flag.
     */
    public static final int Default = 0x0000;

    /**
     * Specifies that the case of the member name should not be considered
     * when binding.
     */
    public static final int IgnoreCase = 0x0001;

    /**
     * Specifies that only members declared at the level of the supplied type's
     * hierarchy should be considered. Inherited members are not considered.
     */
    public static final int DeclaredOnly = 0x0002;

    /**
     * Specifies that instance members are to be included in the search.
     */
    public static final int Instance = 0x0004;

    /**
     * Specifies that static members are to be included in the search.
     */
    public static final int Static = 0x0008;

    /**
     * Specifies that public members are to be included in the search.
     */
    public static final int Public = 0x0010;

    /**
     * Specifies that non-public members are to be included in the search.
     */
    public static final int NonPublic = 0x0020;

    /**
     * Specifies that static members up the hierarchy should be returned.
     * Static members include fields, methods, events, and properties.
     * Nested types are not returned.
     */
    public static final int FlattenHierarchy = 0x0040;

    /**
     * Specifies that a method is to be invoked. This may not be a constructor
     * or a type initializer.
     */
    public static final int InvokeMethod = 0x0100;

    /**
     * Specifies that Reflection should create an instance of
     * the specified type. Calls the constructor that matches
     * the given arguments. The supplied member name is ignored.
     * If the type of lookup is not specified, (Instance | Public)
     * will apply. It is not possible to call a type initializer.
     */
    public static final int CreateInstance = 0x0200;

    /**
     * Specifies that the value of the specified field should be returned.
     */
    public static final int GetField = 0x0400;

    /**
     * Specifies that the value of the specified field should be set.
     */
    public static final int SetField = 0x0800;

    /**
     * Specifies that the value of the specified property should be returned.
     */
    public static final int GetProperty = 0x1000;

    /**
     * Specifies that the value of the specified property should be set.
     * For COM properties, specifying this binding flag is equivalent to
     * specifying PutDispProperty and PutRefDispProperty.
     */
    public static final int SetProperty = 0x2000;

    /**
     * Specifies that the PROPPUT member on a COM object should be invoked.
     * PROPPUT specifies a property-setting function that uses a value.
     * Use PutDispProperty if a property has both PROPPUT and PROPPUTREF
     * and you need to distinguish which one is called.
     */
    public static final int PutDispProperty = 0x4000;


    /**
     * Specifies that the PROPPUTREF member on a COM object should be invoked.
     * PROPPUTREF specifies a property-setting function that uses a reference
     * instead of a value. Use PutRefDispProperty if a property has both
     * PROPPUT and PROPPUTREF and you need to distinguish which one is called.
     */
    public static final int PutRefDispProperty = 0x8000;

    /**
     * Specifies that types of the supplied arguments must exactly match
     * the types of the corresponding formal parameters. Reflection
     * throws an exception if the caller supplies a non-null Binder object,
     * since that implies that the caller is supplying BindToXXX
     * implementations that will pick the appropriate method.
     * Reflection models the accessibility rules of the common type system.
     * For example, if the caller is in the same assembly, the caller
     * does not need special permissions for internal members. Otherwise,
     * the caller needs ReflectionPermission. This is consistent with
     * lookup of members that are protected, private, and so on.
     * The general principle is that ChangeType should perform only
     * widening coercions, which never lose data. An example of a
     * widening coercion is coercing a value that is a 32-bit signed integer
     * to a value that is a 64-bit signed integer. This is distinguished
     * from a narrowing coercion, which may lose data. An example of
     * a narrowing coercion is coercing a 64-bit signed integer to
     * a 32-bit signed integer.
     * The default binder ignores this flag, while custom binders can
     * implement the semantics of this flag.
     */
    public static final int ExactBinding = 0x10000;

    /**
     * Used in COM interop to specify that the return value of the member
     * can be ignored.
     */
    public static final int IgnoreReturn = 0x100000 ;

    /**
     * Returns the set of members whose parameter count matches the number
     * of supplied arguments. This binding flag is used for methods with
     * parameters that have default values and methods with variable arguments
     * (varargs). This flag should only be used with Type.InvokeMember.
     * Parameters with default values are used only in calls where trailing
     * arguments are omitted. They must be the last arguments.
     */
    public static final int OptionalParamBinding = 0x40000;

    /**
     * Not implemented.
     */
    public static final int SuppressChangeType = 0x20000;

    //##########################################################################

}  // class BindingFlags
