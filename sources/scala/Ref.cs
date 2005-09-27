/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $OldId: Ref.java,v 1.2 2002/03/12 13:16:04 zenger Exp $
// $Id$

using System;
using scala.runtime;

namespace scala 
{

	[Meta("class [?T] extends scala.AnyRef;")]
	[Serializable]
	public class Ref : object {

		[Meta("field ?T;")]
		public object elem = null;

		[Meta("constr (?T);")]
		public Ref(object x) 
		{
			elem = x;
		}
	}
}