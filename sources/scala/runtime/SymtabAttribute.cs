using System;

namespace scala.runtime
{
    /// <summary>
    /// Stores the symbol table for every top-level Scala class.
    /// </summary>

    [AttributeUsage(AttributeTargets.Class, AllowMultiple = false, Inherited = false)]
    public class SymtabAttribute : Attribute
    {
        // stores scalac symbol table
        public readonly byte[] symtab;

        // indicates if the type should be considered by the compiler;
        // used for synthetic classes introduced by the Scala compiler
        public readonly bool shouldLoadClass;

        public SymtabAttribute(byte[] symtab)
        {
            this.symtab = symtab;
            this.shouldLoadClass = true;
        }

        public SymtabAttribute() {
            this.symtab = new byte[0];
            this.shouldLoadClass = false;
        }
    }
}
