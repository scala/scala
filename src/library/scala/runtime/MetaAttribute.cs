/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

using System;

namespace scala.runtime
{
    /// <summary>
    /// Stores additional meta-information about classes and members.
    /// Used to augment type information in classes from the scala
    /// library written in Java.
    /// </summary>

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Interface | AttributeTargets.Field
         | AttributeTargets.Constructor | AttributeTargets.Method,
         AllowMultiple = false, Inherited = false)]
    public class MetaAttribute : Attribute
    {
        // keeps a textual representation of the pico-style attributes
        // used in some classes of the runtime library
        public readonly string meta;
        public MetaAttribute(string meta)
        {
            this.meta = meta;
        }
    }
}
