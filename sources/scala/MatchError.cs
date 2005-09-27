/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
**                                                                      **
** $Id$
\*                                                                      */
using System;
using scala.runtime;

namespace scala 
{


	/** This class implements errors which are thrown whenever an
	 *  object doesn't match any pattern of a pattern matching
	 *  expression.
	 *  
	 *  @author  Matthias Zenger
	 *  @version 1.1, 05/03/2004
	 */
	public sealed class MatchError : ApplicationException  
	{
    
		[Meta("constr (System.String, scala.Int);")]
		private MatchError(string source, int line) :
			base(" in '" + source + "' at line " + line)
		{
		}
    
		[Meta("constr (System.String, scala.Int, System.String);")]
		private MatchError(string source, int line, string obj) :
			base("for object " + obj + " in '" + source + "' at line " + line)
		{
		}
    
		[Meta("method [?T](System.String, scala.Int) ?T;")]
		public static object fail(string source, int line) 
		{
			throw new MatchError(source, line);
		}
    
		[Meta("method [?T](System.String, scala.Int, scala.Any) ?T;")]
		public static object report(string source, int line, object obj) 
		{
			try 
			{
				throw new MatchError(source, line, obj.ToString());
			} 
			catch (MatchError e) 
			{
				throw e;
			} 
			catch (Exception e) 
			{
				throw new MatchError(source, line);
			}
		}
	}
}