# Design

## Overview of the User Facing Classes

Here is a diagram showing the relationship between the main collection types:

![Hierarchy](http://www.plantuml.com/plantuml/proxy?src=https://raw.github.com/scala/collection-strawman/master/documentation/hierarchy.plantuml)

The most general collection type is `Iterable[A]`. It provides methods for iterating
over its elements but the order of iteration is not guaranteed to be stable and transformation
operations (e.g. `map`) may not be strictly evaluated.

Then we have `Seq[A]`, `Set[A]` and `Map[K, V]`, which are similar to the current standard
collections.

Last, `View[A]` is a new collection type whose transformation operations are lazy (their
result is not immediately evaluated). Since this type is new it deserves some examples
of use:

~~~ scala
// Strict collection
val xs: List[Int] = 1 :: 2 :: 3 :: Nil

// Lazy view
val ys: View[Int] = xs.view

// The `map` operation is not eagerly evaluated:
// the elements of `ys` are not traversed yet,
// and the resulting `View` just “records” that
// the `map` operation has been called
val zs: View[Int] = ys.map(y => y + 1)

// The evaluation of a `View` happens when a
// “terminal” operation is called (an operation
// that doesn’t return another collection), or
// if it is explicitly converted to a strict collection:
zs.foreach(println) // example of terminal operation
zs.sum              // another example of terminal operation
zs.to(List)         // conversion to a strict collection
~~~

The above diagram shows a simplified view of the hierarchy. In reality we also have
`SortedSet[A]` and `SortedMap[K, V]`, which specialize `Set[A]` and `Map[K, V]`, respectively.
Finally, the whole family is also specialized in two branches `immutable` and `mutable`.

## Implementation Classes

Most of the methods are generically implemented so that they don’t have to be specialized
in each subclass. This is a challenge for methods returning another collection, like
`take` and `map`. We want `take` to return an `Iterable[A]` when invoked on an `Iterable[A]`
and to return a `List[A]` when invoked on a `List[A]`. Said otherwise, the generic
implementation of `take` has to abstract over its return type. Similarly, we want
`map` to return an `Iterable[B]` when invoked on an `Iterable[A]` (given that it’s
invoked with a function `f: A => B`) and to return a `List[B]` when invoked on a `List[A]`.
Said otherwise, the generic implementation of `map` has to abstract over its return type
constructor.

In practice, traits that implement generic operations take three type parameters: the type
of elements `A`, a collection type constructor `CC[_]` and a collection type `C`. The name
of these traits is suffixed by `Ops`, by convention. Here is what the `IterableOps`
definition looks like:

~~~ scala
trait IterableOps[A, CC[_], C] {

  def take(n: Int): C = …
  
  def map[B](f: A => B): CC[B] = …
  
  …
}
~~~

> Each branch of the hierarchy (e.g. `Seq[A]`, `Set[A]`, etc.) has an accompanying `Ops`
trait.

In the case of `List[A]`, the type of elements is `A`, the collection type constructor
is `List` and the collection type is `List[A]`:

~~~ scala
trait List[A]
  extends Seq[A]
    with SeqOps[A, List, List[A]]
~~~

You might be wondering why we need two type parameters `CC[_]` and `C` instead of just
one. In most of the case, the collection type constructor `CC[_]` and the collection
type `C` indeed refer to the same collection class (e.g. `List`) but that’s not always
the case. For instance, the `StringOps` trait is defined as follows:

~~~ scala
trait StringOps extends SeqOps[Char, IndexedSeq, String]
~~~

As you can see, we really need different types to indicate the return type of
transformation operations that return a collection with a possibly different
type of element (like `map`, which returns an `IndexedSeq` in the above case),
and transformation operations that return a collection with the same type of
element (like `take`, which returns a `String` in the above case).

## Specific Overloads

As mentioned above the type constructor used for transformation operations like
`map`, in `StringOps`, is `IndexedSeq`. It means that if you write the
following:

~~~ scala
val s = "foo".map(c => c.toUpper)
~~~

Then `s` would have type `IndexedSeq[Char]` because the function passed to `map`
returns a `Char`. However, in the above code we really want `s` to have type
`String`. That’s why we actually defined, in `StringOps`, an overload of `map`
that returns a `String` in the special case where the supplied function returns
a `Char`:

~~~ scala
def map(f: Char => Char): String
~~~

Even though `StringOps` also inherits from `IterableOps`, which defines the
original `map` method, when one calls `map` on a `String`, the compiler
first checks if the `map` overload defined in `StringOps` matches the invocation,
otherwise it fallbacks to the more general `map` definition.

## Four Kinds of Factories

Method overloading is also used by sorted collections. These collections
define their order of iteration based on an `Ordering` instance for their
type of element. Consequently, a sorted collection can only be built
if it is supplied such an `Ordering` instance. However, the standard definition
of `map`, in `IterableOps` doesn’t provide an `Ordering` instance:

~~~ scala
trait IterableOps[A, CC[_], C] {

  def map[B](f: A => B): CC[B]

}
~~~

That’s why sorted collections add overloads of these methods to also take
an implicit `Ordering`:

~~~ scala
trait SortedSetOps[A, CC[_], C]
  extends SetOps[A, Set, C] {
  
  def map[B](f: A => B)(implicit ord: Ordering[B]): CC[B]
  
}
~~~

The same trick is used for `Map[K, V]`. We add an overload of `map` that
returns a `CC[K2, V2]` if the supplied function returns a pair `(K2, V2)`:

~~~ scala
trait MapOps[K, V, CC[_, _], C]
  extends IterableOps[(K, V), Iterable, C] {
  
  def map[K2, V2](f: ((K, V)) => (K2, V2)): CC[K2, V2]
  
}
~~~

And, finally, this trick is also used for sorted Map collections:

~~~ scala
trait SortedMapOps[K, V, CC[_, _], C]
  extends MapOps[K, V, Map, C] {
  
  def map[K2, V2](f: ((K, V)) => (K2, V2))(implicit ord: Ordering[K2]): CC[K2, V2]
  
}
~~~

We see that we have four kinds of collections:

- `Iterable[A]`, takes a single unconstrained, type parameter `A`,
- `SortedSet[A]`, takes a single constrained type parameter `A`,
- `Map[K, V]`, takes two unconstrained type parameters, `K` and `V`,
- `SortedMap[K, V]`, takes a constrained type parameter `K` and
  and an unconstrained type parameter `V`.

You might also want to have a look at a related blog post:
[Tribulations of `CanBuildFrom`](http://scala-lang.org/blog/2017/05/30/tribulations-canbuildfrom.html).

## View-Based Default Implementations

Most of the collection operations have a default implementation. This is
convenient because it makes it easier to extend the hierarchy with a
new collection type without having to implement a lot of methods.

For operations that return another collection (e.g. `map` or `take`),
two approaches are possible:

1. start with an empty target collection and push elements to it;
2. create a lazy view of the result and then convert this view to
   the target collection

What are the differences between these two strategies? For strict collections,
they are equivalent. But if we try to apply the first approach to a lazy
collection type (e.g. `LazyList`, formerly known as `Stream` in Scala 2.12),
then we realize that it makes no sense: when we call `map` on a `LazyList`
we don’t want to create a new empty `LazyList`, and then iterate on the
receiver to push elements to the resulting `LazyList`. We see that the first
approach is fundamentally incompatible with lazy collections. The second
approach, on the other hand, is lazy collection friendly: we start by creating
a view of the result and then convert this view to the target collection.
If the target collection is a strict collection, then we just evaluate the
view. If the target collection is a lazy collection, then the conversion operation
does not evaluate the elements of the view (so that laziness is preserved).

In other words, the second approach makes it possible to define default
implementations that work for both strict and lazy collections.

As an example, here is how the default implementation of `map` and `take`
are defined:

~~~ scala
def take(n: Int): C = fromSpecificIterable(View.Take(toIterable, n))
def map[B](f: A => B): CC[B] = fromIterable(View.Map(toIterable, f))
~~~

`View.Map` creates a `View` that applies the function `f` to the elements
of an underlying collection, and `View.Take` creates a `View` that takes
only the `n` first elements of an underlying collection.

`fromSpecificIterable`, `fromIterable` and `toIterable` are abstract methods
defined as follows:

~~~ scala
trait IterableOps[A, CC[_], C] {

  def toIterable: Iterable[A]

  protected[this] def fromSpecificIterable(it: Iterable[A]): C

  protected[this] def fromIterable[E](it: Iterable[E]): CC[E]

}
~~~

These methods are then implemented by concrete collection types.

`fromSpecificIterable` defines how a to build a collection `C` from a
collection having the same type of elements `A`, and `fromIterable`
defines how to build a collection `CC[E]` from a collection having a
different type of elements `E`.

### Four Kinds of `fromIterable`

As previously explained, we have four kinds of collections and
each collection kind provides its own conversion methods:

~~~ scala
trait SortedSetOps[A, CC[_], C] {

  protected[this] def sortedFromIterable[B : Ordering](it: Iterable[B]): CC[B]

}

trait MapOps[K, V, CC[_, _], C] {

  protected[this] def mapFromIterable[K2, V2](it: Iterable[(K2, V2)]): CC[K2, V2]

}

trait SortedMapOps[K, V, CC[_, _], C] {

  protected[this] def sortedMapFromIterable[K2 : Ordering, V2](it: collection.Iterable[(K2, V2)]): CC[K2, V2]

}
~~~

### Optimizations for Strict Collections

In some cases the view-based implementation can be significantly less efficient
for strict collections than a builder-based implementation. A good example is
the `partition` operation:

~~~ scala
def partition(p: A => Boolean): (C, C)
~~~

This method returns two collections, one with elements that satisfy the predicate
`p` and one with elements that don’t.

A view-based implementation of this operation has no other choice than performing
two traversals of the receiver collection. On the other hand, a builder-based
solution would just require one traversal in which it is possible to build the
two resulting collections.

In order to get better performance on strict collections, we introduced traits that
override operation implementations using a builder-based solution. The names of
these traits are (by convention) prefixed with `StrictOptimized`:

~~~ scala
trait StrictOptimizedIterableOps[A, CC[_], C]
  extends IterableOps[A, CC, C] {

  override def partition(p: A => Boolean): (C, C) = {
    val l, r = newSpecificBuilder()
    toIterable.iterator().foreach(x => (if (p(x)) l else r) += x)
    (l.result(), r.result())
  }
  
}
~~~

Note that the `newSpecificBuilder` method, used to get a `Builder[A, C]`, is
defined in `IterableOps` but it’s usage is not recommended: default operation
implementations should always preserve laziness (by being view-based).
