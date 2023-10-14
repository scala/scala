import p._
package p { class X extends C } // not ambiguous (compiles without the import)
package q { class Y extends C } // requires the import
