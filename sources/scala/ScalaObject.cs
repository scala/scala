/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

using System;
using scala.runtime;

namespace scala 
{
	[Meta("class extends scala.AnyRef;")]
	public interface ScalaObject 
	{
		/** This method is needed for optimizing pattern matching expressions
		 *  which match on constructors of case classes.
		 */
		int __tag();
	}
}