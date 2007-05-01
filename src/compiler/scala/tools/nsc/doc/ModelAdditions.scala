/* NSC -- new Scala compiler
 * Copyright 2007-2008 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc.doc

/**
 *  @author Sean McDirmid
 *  @version 1.0
 */
class ModelAdditions(val global : Global) {
  import global._;
  import definitions._;
  def addition(sym : global.Symbol) : Unit = {}

  addition(AllClass);
  comments(AllClass) = """
    /** <p>
     *    Class <code>Nothing</code> (previously named <code>All</code> in
     *    <a href="http://scala-lang.org" target="_top">Scala</a> 2.2.0 and
     *    older versions) is - together with class <a href="Null.html">
     *    <code>Null</code></a> - at the bottom of the
     *    <a href="http://scala-lang.org" target="_top">Scala</a> type
     *    hierarchy.
     *  </p>
     *  <p>
     *    Type <code>Nothing</code> is a subtype of every other type
     *    (including <a href="Null.html"><code>Null</code></a>); there
     *    exist <em>no instances</em> of this type. Even though type
     *    <code>Nothing</code> is empty, it is nevertheless useful as a
     *    type parameter. For instance, the <a href="http://scala-lang.org"
     *    target="_top">Scala</a> library defines a value
     *    <a href="Nil$object.html"><code>Nil</code></a> of type
     *    <code><a href="List.html">List</a>[Nothing]</code>. Because lists
     *    are covariant in <a href="http://scala-lang.org" target="_top">Scala</a>,
     *    this makes <a href="Nil$object.html"><code>Nil</code></a> an
     *    instance of <code><a href="List.html">List</a>[T]</code>, for
     *    any element type <code>T</code>.
     *  </p>
     */""";
   addition(AllRefClass);
   comments(AllRefClass) = """
     /** <p>
      *    Class <code>Null</code> (previously named <code>AllRef</code> in
      *    <a href="http://scala-lang.org" target="_top">Scala</a> 2.2.0 and
      *    older versions) is - together with class <a href="Nothing.html">
      *    <code>Nothing</code> - at the bottom of the
      *    <a href="http://scala-lang.org" target="_top">Scala</a> type
      *    hierarchy.
      *  </p>
      *  <p>
      *    Type <code>Null</code> is a subtype of all reference types; its
      *    only instance is the <code>null</code> reference.
      *    Since <code>Null</code> is not a subtype of value types,
      *    <code>null</code> is not a member of any such type. For instance,
      *    it is not possible to assign <code>null</code> to a variable of
      *    type <a href="Int.html"><code>Int</code></a>.
      * </p>
      */""";

   addition(AnyClass);
   comments(AnyClass) = """
   /** <p>
    *    Class <code>Any</code> is the root of the <a
    *    href="http://scala-lang.org/"
    *    target="_top">Scala</a> class hierarchy. Every class in a
    *    <a href="http://scala-lang.org/" target="_top">Scala</a> execution
    *    environment inherits directly or indirectly from this class.
    *    Class <code>Any</code> has two direct subclasses:
    *    <a href="AnyRef.html"><code>AnyRef</code></a> and
    *    <a href="AnyVal.html"><code>AnyVal</code></a>.
    *  </p>
    */""";
  /****/
  addition(Object_isInstanceOf);

  comments(Object_isInstanceOf) = """
  /** <p>
   *    The method <code>isInstanceOf</code> is the pendant of the Java
   *    operator <code>instanceof</code>.
   *  </p>
   *  @see <ul><li>Java Language Specification (2<sup>nd</sup> Ed.):
   *      <a href="http://java.sun.com/docs/books/jls/second_edition/html/expressions.doc.html#80289"
   *      target="_top">Operator <code>instanceof</code></a>.</li></ul>
   */""";
   /****/
   addition(Object_synchronized);
   comments(Object_synchronized) = """
   /** <p>
    *    To make your programs thread-safe, you must first identify what
    *    data will be shared across threads. If you are writing data that
    *    may be read later by another thread, or reading data that may
    *    have been written by another thread, then that data is shared,
    *    and you must synchronize when accessing it.
    *  </p>
    *  @see <ul><li>The Java Tutorials:
    *      <a href="http://java.sun.com/docs/books/tutorial/essential/concurrency/sync.html"
    *      target="_top">Synchronization</a>.</li>
    *      <li> IBM developerWorks:
    *      <a href="http://www-128.ibm.com/developerworks/java/library/j-threads1.html"
    *      target="_top">Synchronization is not the enemy</a>.</li></ul>
    */""";
   addition(AnyRefClass);
   comments(AnyRefClass) = """
   /** <p>
    *    Class <code>AnyRef</code> is the root class of all
    *    <em>reference types</em>.
    *  </p>
    */""";
   addition(AnyValClass);
   comments(AnyValClass) = """
     /** <p>
    *    Class <code>AnyVal</code> is the root class of all
    *    <em>value types</em>.
    *  </p>
    *  <p>
    *    <code>AnyVal</code> has a fixed number subclasses, which
    *    describe values which are not implemented as objects in the
    *    underlying host system.
    *  </p>
    *  <p>
    *    Classes <a href="Double.html"><code>Double</code></a>,
    *    <a href="Float.html"><code>Float</code></a>,
    *    <a href="Long.html"><code>Long</code></a>,
    *    <a href="Int.html"><code>Int</code></a>,
    *    <a href="Char.html"><code>Char</code></a>,
    *    <a href="Short.html"><code>Short</code></a>, and
    *    <a href="Byte.html"><code>Byte</code></a> are together called
    *    <em>numeric value types</em>.
    *    Classes <a href="Byte.html"><code>Byte</code></a>,
    *    <a href="Short.html"><code>Short</code></a>, or
    *    <a href="Char.html"><code>Char</code></a>
    *    are called <em>subrange types</em>. Subrange types, as well as
    *    <a href="Int.html"><code>Int</code></a> and
    *    <a href="Long.html"><code>Long</code></a> are called
    *    <em>integer types</em>, whereas
    *    <a href="Float.html"><code>Float</code></a> and
    *    <a href="Double.html"><code>Double</code></a> are called
    *    <em>floating point types</em>.
    *  </p>
    */""";
   addition(BooleanClass);
   comments(BooleanClass) = {"""
   /** <p>
    *    Class <code>Boolean</code> has only two values: <code>true</code>
    *    and <code>false</code>.
    *  </p>
    */"""};
   def numericValDescr(sym: Symbol) = {
     val maxValue = "MAX_" + sym.name.toString().toUpperCase()
     val minValue = "MIN_" + sym.name.toString().toUpperCase();
     addition(sym);
     comments(sym) = """
     /** <p>
      *    Class <code>""" + sym.name + """ </code> belongs to the value
      *    classes whose instances are not represented as objects by the
      *    underlying host system. All value classes inherit from class
      *    <a href="AnyVal.html"><code>AnyVal</code></a>.
      *  </p>
      *  <p>
      *    Values <code>""" + maxValue + """</code> and <code>""" + minValue + """</code>
      *    are in defined in object <a href="Math$object.html">scala.Math</a>.
      *  </p>
      */""";
   }
   (ByteClass :: CharClass :: DoubleClass :: LongClass ::
    FloatClass :: IntClass :: ShortClass :: Nil).foreach(numericValDescr);

   addition(UnitClass);
   comments(UnitClass) = {"""
   /** <p>
    *    Class <code>Unit</code> has only one value: <code>()</code>.
    *  </p>
    */"""};
   addition(UnitClass);

   def boxedValDescr(what : String) = {
     val sym = definitions.getClass("java.lang." + what);
     addition(sym);
     comments(sym) = """
     /** <p>
      *    Class <code>""" + sym.name + """</code> implements the
      *    boxing/unboxing from/to value types.
      *  </p>
      *  <p>
      *    Boxing and unboxing enable value types to be treated as objects;
      *    they provide a unified view of the type system wherein a value
      *    of any type can ultimately be treated as an object.
      *  </p>
      */"""
   };
   ("Float" :: "Long" :: "Number" :: "Integer" :: Nil).foreach(boxedValDescr);

   object exceptions extends collection.jcl.TreeMap[String,(Symbol,String)] {
     def f(name : String) = {
       this("Predef." + name) = (definitions.PredefModule, name);
     }
     f("IndexOutOfBoundsException");
     f("NoSuchElementException");
     f("NullPointerException");
     f("UnsupportedOperationException");
   }
}
