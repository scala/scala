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
        public readonly string meta;
        public MetaAttribute(string meta)
        {
            this.meta = meta;
        }
    }
}