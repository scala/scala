/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */

// $Id: EmitUtils.java 168 2005-12-12 14:20:06Z mihaylov $

package ch.epfl.lamp.compiler.msil.emit

import ch.epfl.lamp.compiler.msil.*

/**
 * This class holds some usefull methods or fields which are not in the
 * Reflection or Reflection.Emit Namespace (see .NET documentation)
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object EmitUtils {

    //##########################################################################

    /**
     * Defines a dynamic assembly with the specified name.
     */
    def DefineDynamicAssembly(name: AssemblyName): AssemblyBuilder = new AssemblyBuilder(name)

    /**
     * Helper functions that adds a space only after non-empty string
     */
    def append(sb: StringBuffer, str: String): StringBuffer = {
	if (sb.length() > 0) {
	    var last: Char = sb.charAt(sb.length() - 1)
	    if (last != ' ' && last != '\t' || last != '\n')
		sb.append(" ")
	}
	//return sb.append(str)
	return trim(sb.append(str))
    }

    /**
     * Helper functions that adds a space only after non-empty string
     */
    def append(str: StringBuffer, o: Object): String = append(str, "" + o)
  
    /**
     * Helper function that removes the trailing spaces
     */
    def trim(sb: StringBuffer): StringBuffer = {
	while ((sb.length() > 0) && (sb.charAt(sb.length() - 1) == ' '))
	    sb.deleteCharAt(sb.length() - 1)
	return sb
    }

    //##########################################################################
}
