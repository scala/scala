using System;

namespace scala.runtime
{
    /// <summary>
    /// Stores the symbol table for every top-level Scala class.
    /// </summary>

    [AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = false)]
    public class SymtabAttribute : Attribute
    {
        public readonly byte[] symtab;
        public SymtabAttribute(byte[] symtab)
        {
            this.symtab = symtab;
        }
    }
}
