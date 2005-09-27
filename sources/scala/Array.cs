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

	[Meta("class [?T] extends scala.AnyRef;")]
	[Serializable]
	public abstract class Array /*: Cloneable*/
	{

		[Meta("constr (scala.Int);")]
		public Array() {}

		[Meta("method []scala.Array[?T];")]
		public abstract object value();

		[Meta("method []scala.Int;")]
		public abstract int length();
		[Meta("method (scala.Int)?T;")]
		public abstract object apply(int i);
		[Meta("method (scala.Int,?T)scala.Unit;")]
		public abstract void update(int i, object x);
	}
}
