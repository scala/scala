/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:Unit.cs 5359 2005-12-16 16:33:49 +0100 (Fri, 16 Dec 2005) dubochet $

using System;
using scala.runtime;

namespace scala 
{

	public abstract class Unit : AnyVal {

		public  void    value() {}

		public Unit() {}

		public override bool Equals(object other) 
		{
			return other is Unit;
		}
		public override int GetHashCode() 
		{
			int  bits = 4041;
			return bits;
		}
		public override string ToString() 
		{
			return "()";
		}

		[Meta("method (scala.Any)scala.Boolean;")]
		public bool __eq__eq  (object other) { return  Equals(other); }
		[Meta("method (scala.Any)scala.Boolean;")]
		public bool __bang__eq(object other) { return !Equals(other); }

		public string  __plus      (string  that) { return  this  +  that; }

	}
}