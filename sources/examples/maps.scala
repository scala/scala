module maps {

  trait MapStruct[kt, vt] {
    trait Map extends Function1[kt, vt] with {
      def extend(key: kt, value: vt): Map;
      def remove(key: kt): Map;
      def domain: Stream[kt];
      def range: Stream[vt];
    }
    type map <: Map;
    val empty: map;
  }

  class AlgBinTree[kt <: Ord[kt], vt <: AnyRef]() extends MapStruct[kt, vt] {
    type map = AlgMap;

    val empty: AlgMap = Empty();

    private case class
      Empty() extends AlgMap {},
      Node(key: kt, value: vt, l: map, r: map) extends AlgMap {}

    trait AlgMap extends Map {
      def apply(key: kt): vt = this match {
	case Empty() => null
	case Node(k, v, l, r) =>
	  if (key < k) l.apply(key)
	  else if (key > k) r.apply(key)
	  else v
      }

      def extend(key: kt, value: vt): map = this match {
	case Empty()=> Node(key, value, empty, empty)
	case Node(k, v, l, r) =>
	  if (key < k) Node(k, v, l.extend(key, value), r)
	  else if (key > k) Node(k, v, l, r.extend(key, value))
	  else Node(k, value, l, r)
      }

      def remove(key: kt): map = this match {
	case Empty()=> empty
	case Node(k, v, l, r) =>
	  if (key < k) Node(k, v, l.remove(key), r)
	  else if (key > k) Node(k, v, l, r.remove(key))
	  else if (l == empty) r
	  else if (r == empty) l
	  else {
	    val midKey = r.domain.head;
	    Node(midKey, r.apply(midKey), l, r.remove(midKey))
	  }
      }

      def domain: Stream[kt] = this match {
	case Empty()=> Stream.empty
	case Node(k, v, l, r) => l.domain append Stream.cons(k, r.domain)
      }

      def range: Stream[vt] = this match {
	case Empty()=> Stream.empty
	case Node(k, v, l, r) => l.range append Stream.cons(v, r.range)
      }
    }
  }

  class OOBinTree[kt <: Ord[kt], vt <: AnyRef]() extends MapStruct[kt, vt] {
    type map = OOMap;

    trait OOMap extends Map with {
      def apply(key: kt): vt;
      def extend(key: kt, value: vt): map;
      def remove(key: kt): map;
      def domain: Stream[kt];
      def range: Stream[vt];
    }
    val empty: OOMap = new OOMap {
      def apply(key: kt): vt = null;
      def extend(key: kt, value: vt) = new Node(key, value, empty, empty);
      def remove(key: kt) = empty;
      def domain: Stream[kt] = Stream.empty;
      def range: Stream[vt] = Stream.empty;
    }
    private class Node(k: kt, v: vt, l: map, r: map) extends OOMap with {
      def apply(key: kt): vt =
	if (key < k) l.apply(key)
	else if (key > k) r.apply(key)
	else v;
      def extend(key: kt, value: vt): map =
	if (key < k) new Node(k, v, l.extend(key, value), r)
	else if (key > k) new Node(k, v, l, r.extend(key, value))
	else new Node(k, value, l, r);
      def remove(key: kt): map =
	if (key < k) new Node(k, v, l.remove(key), r)
	else if (key > k) new Node(k, v, l, r.remove(key))
	else if (l == empty) r
	else if (r == empty) l
	else {
	  val midKey = r.domain.head;
	  new Node(midKey, r(midKey), l, r.remove(midKey))
	}
      def domain: Stream[kt] = l.domain append Stream.cons(k, r.domain);
      def range: Stream[vt] = l.range append Stream.cons(v, r.range);
    }
  }

  class MutBinTree[kt <: Ord[kt], vt <: AnyRef]() extends MapStruct[kt, vt] {
    type map = MutMap;
    class MutMap(key: kt, value: vt) extends Map with {
      val k = key;
      var v = value;
      var l = empty, r = empty;

      def apply(key: kt): vt =
	if (this == empty) null
	else if (key < k) l.apply(key)
	else if (key > k) r.apply(key)
	else v;

      def extend(key: kt, value: vt): map =
	if (this == empty) new MutMap(key, value)
	else {
	  if (key < k) l = l.extend(key, value)
	  else if (key > k) r = r.extend(key, value)
	  else v = value;
	  this
	}

      def remove(key: kt): map =
	if (this == empty) this
	else if (key < k) { l = l.remove(key) ; this }
	else if (key > k) { r = r.remove(key) ; this }
	else if (l == empty) r
	else if (r == empty) l
	else {
	  var mid = r;
	  while (!(mid.l == empty)) { mid = mid.l }
	  mid.r = r.remove(mid.k);
	  mid.l = l;
	  mid
	}
      def domain: Stream[kt] =
	if (this == empty) Stream.empty;
	else l.domain append Stream.cons(k, r.domain);
      def range: Stream[vt] =
	if (this == empty) Stream.empty;
	else l.range append Stream.cons(v, r.range);
    }
    val empty = new MutMap(null, null);
  }
}



