Conventions for Collection Implementors

Martin Odersky
19 Mar 2010

This note describes some conventions which must be followed to keep
the collection libraries consistent.

We distinguish in the following between two kinds of methods

 - ``Accessors'' access some of the elements of a collection, but return a result which
   is unrelated to the collection. 
   Example of accessors are: head, foldLeft, indexWhere, toSeq.

 - ``Transformers'' access elements of a collection and produce a new collection of related
   type as a result. The relation might either be direct (same type as receiver)
   or indirect, linked by a CanBuildFrom implicit.
   Example of transformers are: filter, map, groupBy, zip.

1. Proxies

Every collection type has a Proxy class that forwards all operations to
an underlying collection. Proxy methods are all implemented in classes
with names ending in `ProxyLike'. If you add a new method to a collection 
class you need to add the same method to the corresponding ProxyLike class.

2. Forwarders

Classes Traversable, Iterable, and Seq also have forwarders, which
forward all collection-specific accessor operations to an underlying
collection. These are defined as classes with names ending
in `Forwarder' in package collection.generic.  If you add a new
accessor method to a Seq or one of its collection superclasses, you
need to add the same method to the corresponding forwarder class.

3. Views

Classes Traversable, Iterable, Seq, IndexedSeq, and mutable.IndexedSeq
support views. Their operations are all defined in classes with names
ending in `ViewLike'. If you add a new transformer method to one of
the above collection classes, you need to add the same method to the
corresponding view class. Failure to do so will cause the
corresponding method to fail at runtime with an exception like
UnsupportedOperationException("coll.newBuilder"). If there is no good
way to implement the operation in question lazily, there's a fallback
using the newForced method. See the definition of sorted in trait
SeqViewLike as an example.


 
