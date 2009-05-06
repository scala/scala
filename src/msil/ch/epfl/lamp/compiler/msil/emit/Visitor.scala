/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */

// $Id: Visitor.java 14655 2008-04-15 09:37:02Z lorch $

package ch.epfl.lamp.compiler.msil.emit

import java.io.IOException

/**
 * The Visitor interface to walk through the MSIL code Builder hierarchy.
 */
trait Visitor {

    //##########################################################################

    /** Visit an AssemblyBuilder */
    @throws(classOf[IOException])
    def caseAssemblyBuilder(assemblyBuilder: AssemblyBuilder): Unit

    /** Visit a ModuleBuilder */
    @throws(classOf[IOException])
    def caseModuleBuilder(moduleBuilder: ModuleBuilder): Unit

    /** Visit a TypeBuilder */
    @throws(classOf[IOException])
    def caseTypeBuilder(typeBuilder: TypeBuilder): Unit

    /** Visit a FieldBuilder */
    @throws(classOf[IOException])
    def caseFieldBuilder(fieldBuilder: FieldBuilder): Unit

    /** Visit a ConstructorBuilder */
    @throws(classOf[IOException])
    def caseConstructorBuilder(constructorBuilder: ConstructorBuilder): Unit

    /** Visit a MethodBuilder */
    @throws(classOf[IOException])
    def caseMethodBuilder(methodBuilder: MethodBuilder): Unit

    /** Visit a ParameterBuilder */
    @throws(classOf[IOException])
    def caseParameterBuilder(parameterBuilder: ParameterBuilder): Unit

    /** Visit an ILGenerator */
    @throws(classOf[IOException])
    def caseILGenerator(iLGenerator: ILGenerator): Unit

    /** Visit an OpCode */
    @throws(classOf[IOException])
    def caseOpCode(opCode: OpCode): Unit

    /** Visit a LocalBuilder */
    @throws(classOf[IOException])
    def caseLocalBuilder(localBuilder: LocalBuilder): Unit

    //##########################################################################
}
