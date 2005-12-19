/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:Boolean.cs 5359 2005-12-16 16:33:49 +0100 (Fri, 16 Dec 2005) dubochet $

using System;
using scala.runtime;

namespace scala 
{

	[Meta("class extends scala.AnyVal;")]
	[Serializable]
	public abstract class Boolean : AnyVal 
	{

		public readonly bool value;

		public Boolean(bool value) 
		{
			this.value = value;
		}

		public override bool Equals(object other) 
		{
			return other is Boolean && value == ((Boolean)other).value;
		}
		public override int GetHashCode() 
		{
			return  value ? 1231 : 1237;
		}
		public override string ToString() 
		{
			return value.ToString();
		}

		[Meta("method (scala.Any)scala.Boolean;")]
		public bool __eq__eq  (object other) { return  Equals(other); }
		[Meta("method (scala.Any)scala.Boolean;")]
		public bool __bang__eq(object other) { return !Equals(other); }

		[Meta("method []scala.Boolean;")]
		public bool __bang      (            ) { return !value        ; }

		public string  __plus      (string  that) { return  value +  that; }

		public bool __eq__eq     (bool that) { return  value == that; }
		public bool __bang__eq   (bool that) { return  value != that; }
		public bool __bar__bar   (bool that) { return  value || that; }
		public bool __amp__amp   (bool that) { return  value && that; }
		public bool __bar       (bool that) { return  value |  that; }
		public bool __amp       (bool that) { return  value &  that; }
		public bool __up        (bool that) { return  value ^  that; }

	}
}