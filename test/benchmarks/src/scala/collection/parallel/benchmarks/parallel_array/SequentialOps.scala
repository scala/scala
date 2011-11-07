package scala.collection.parallel.benchmarks.parallel_array



trait SequentialOps[T] {
  
  var arr: Array[Any] = null
  
  def sequentialReduce(op: (T, T) => T, sz: Int, init: T) = {
    var i = 0
    val until = sz
    var sum = init
    while (i < until) {
      sum = op(sum, arr(i).asInstanceOf[T])
      i += 1
    }
    sum
  }
  
  def sequentialScan(z: T, op: (T, T) => T, sz: Int) = {
    var outarr = new Array[Any](sz + 1)
    outarr(0) = z
    var last = z
    var i = 0
    var j = 1
    val until = sz
    while (i < until) {
      last = op(last, arr(i).asInstanceOf[T])
      outarr(j) = last
      i += 1
      j += 1
    }
  }
  
  def sequentialCount(pred: T => Boolean, sz: Int) = {
    var i = 0
    val until = sz
    var sum = 0
    while (i < until) {
      if (pred(arr(i).asInstanceOf[T])) sum += 1
      i += 1
    }
    sum
  }
  
  def sequentialForeach[U](f: T => U, sz: Int) = {
    var i = 0
    val until = sz
    var sum = 0
    while (i < until) {
      f(arr(i).asInstanceOf[T])
      i += 1
    }
  }
  
  def sequentialSum[U >: T](sz: Int)(implicit num: Numeric[U]) = {
    var i = 0
    val until = sz
    var sum = num.zero
    while (i < until) {
      sum = num.plus(sum, arr(i).asInstanceOf[T])
      i += 1
    }
    sum
  }
  
  def sequentialMin[U >: T](sz: Int)(implicit ord: Ordering[U]) = {
    var i = 1
    val until = sz
    var min = arr(0).asInstanceOf[U]
    while (i < until) {
      val elem = arr(i).asInstanceOf[U]
      if (ord.lt(elem, min)) min = elem
      i += 1
    }
    min
  }
  
  def sequentialForall(pred: T => Boolean, sz: Int) = {
    var i = 0
    val until = sz
    var all = true
    while (i < until) { 
      if (pred(arr(i).asInstanceOf[T])) i += 1
      else {
        all = false
        i = until
      }
    }
    all
  }

  def sequentialExists(pred: T => Boolean, sz: Int) = {
    var i = 0
    val until = sz
    var some = false
    while (i < until) { 
      if (pred(arr(i).asInstanceOf[T])) {
        some = true
        i = until
      } else i += 1
    }
    some
  }
  
  def sequentialFind(pred: T => Boolean, sz: Int) = {
    var i = 0
    val until = sz
    var opt: Option[T] = None
    while (i < until) { 
      if (pred(arr(i).asInstanceOf[T])) {
        opt = Some(arr(i).asInstanceOf[T])
        i = until
      } else i += 1
    }
    opt
  }
  
  def sequentialFilter(pred: T => Boolean, sz: Int) = {
    var i = 0
    val buff = new collection.mutable.ArrayBuffer[T]
    while (i < sz) {
      val elem = arr(i).asInstanceOf[T]
      if (pred(elem)) buff += elem
      i += 1
    }
    val resarr = new Array[Any](buff.size)
    buff.copyToArray(resarr, 0)
    resarr
  }
  
  def sequentialPartition(pred: T => Boolean, sz: Int) = {
    var i = 0
    val btrue = new collection.mutable.ArrayBuffer[T]
    val bfalse = new collection.mutable.ArrayBuffer[T]
    while (i < sz) {
      val elem = arr(i).asInstanceOf[T]
      if (pred(elem)) btrue += elem
      else bfalse += elem
      i += 1
    }
    val restrue = new Array[Any](btrue.size)
    val resfalse = new Array[Any](bfalse.size)
    btrue.copyToArray(restrue, 0)
    bfalse.copyToArray(resfalse, 0)
    (restrue, resfalse)
  }
  
  def sequentialTakeOpt(n: Int, sz: Int) = {
    var i = 0
    val until = if (n < sz) n else sz
    val res = new Array[Any](until)
    Array.copy(arr, 0, res, 0, until)
//    while (i < until) {
//      res(i) = arr(i)
//      i += 1
//    }
    res
  }

  def sequentialTake(n: Int, sz: Int) = {
    var i = 0
    val b = new collection.mutable.ArrayBuffer[T]
    val until = if (n < sz) n else sz
    b.sizeHint(until)
    while (i < until) {
      val elem = arr(i).asInstanceOf[T]
      b += elem
      i += 1
    }
    val res = new Array[Any](n)
    b.copyToArray(res, 0)
    res
  }
  
  def sequentialDrop(n: Int, sz: Int) = {
    var i = n
    val b = new collection.mutable.ArrayBuffer[T]
    b.sizeHint(sz - n)
    while (i < sz) {
      val elem = arr(i).asInstanceOf[T]
      b += elem
      i += 1
    }
    val res = new Array[Any](n)
    b.copyToArray(res, 0)
    res
  }
  
  def sequentialSlice(from: Int, until: Int, sz: Int) = {
    var i = from
    val b = new collection.mutable.ArrayBuffer[T]
    b.sizeHint(until - from)
    while (i < until) {
      val elem = arr(i).asInstanceOf[T]
      b += elem
      i += 1
    }
    val res = new Array[Any](until - from)
    b.copyToArray(res, 0)
    res
  }
  
  def sequentialSplitAtOpt(n: Int, sz: Int) = {
    var i = 0
    val before = new Array[Any](n)
    val after = new Array[Any](sz - n)
    Array.copy(arr, 0, before, 0, n)
    Array.copy(arr, n, after, 0, sz - n)
    (before, after)
  }
  
  def sequentialSplitAt(n: Int, sz: Int) = {
    var i = 0
    val before = new collection.mutable.ArrayBuffer[T]
    before.sizeHint(n)
    val after = new collection.mutable.ArrayBuffer[T]
    after.sizeHint(sz - n)
    while (i < sz) {
      if (i < n) before += arr(i).asInstanceOf[T]
      else after += arr(i).asInstanceOf[T]
      i += 1
    }
    val resbef = new Array[Any](n)
    val resaft = new Array[Any](sz - n)
    before.copyToArray(resbef, 0)
    after.copyToArray(resaft, 0)
    (resbef, resaft)
  }
  
  def sequentialTakeWhile(p: T => Boolean, sz: Int) = {
    var i = 0
    val b = new collection.mutable.ArrayBuffer[T]
    while (i < sz) {
      val elem = arr(i).asInstanceOf[T]
      if (p(elem)) {
        b += elem
        i += 1
      } else i = sz
    }
    val res = new Array[Any](sz)
    b.copyToArray(res, 0)
    res
  }
  
  def sequentialSpan(p: T => Boolean, sz: Int) = {
    val bpref = new collection.mutable.ArrayBuffer[T]
    val brest = new collection.mutable.ArrayBuffer[T]
    var i = 0
    var prefix = true
    var pos = sz
    while (i < sz) {
      val elem = arr(i).asInstanceOf[T]
      if (prefix) {
        if (p(elem)) bpref += elem
        else {
          pos = i
          prefix = false
          brest += elem
        }
      } else brest += elem
      i += 1
    }
    val respref = new Array[Any](pos)
    val resrest = new Array[Any](sz - pos)
    bpref.copyToArray(respref, 0)
    brest.copyToArray(resrest, 0)
    (respref, resrest)
  }
  
  def sequentialMap(f: T => T, sz: Int) = {
    val b = new collection.mutable.ArrayBuffer[T](sz)
    
    var i = 0
    while (i < sz) {
      b += f(arr(i).asInstanceOf[T])
      i += 1
    }
    
    val res = new Array[Any](sz)
    b.copyToArray(res, 0)
    res
  }
  
  def sequentialMapOpt(f: T => T, sz: Int) = {
    val res = new Array[Any](sz)
    
    var i = 0
    while (i < sz) {
      res(i) = f(arr(i).asInstanceOf[T])
      i += 1
    }
    
    res
  }
  
  def sequentialPartialMap(f: PartialFunction[T, T], sz: Int) = {
    val b = new collection.mutable.ArrayBuffer[T](sz)
    
    var i = 0
    while (i < sz) {
      val elem = arr(i).asInstanceOf[T]
      if (f.isDefinedAt(elem)) b += f(elem)
      i += 1
    }
    
    val res = new Array[Any](b.size)
    b.copyToArray(res, 0)
    res
  }
  
  def sequentialFlatMap(f: T => Traversable[Int], sz: Int) = {
    val b = new collection.mutable.ArrayBuffer[Int](sz)
    
    var i = 0
    while (i < sz) {
      val ts = f(arr(i).asInstanceOf[T])
      for (elem <- ts) b += elem
      i += 1
    }
    
    val res = new Array[Any](b.size)
    b.copyToArray(res, 0)
    res
  }  
  
  def sequentialCopyToArray(destarr: Array[Any], pos: Int, sz: Int) = {
    Array.copy(arr, 0, destarr, pos, sz)
  }
  
  def sequentialSegmentLength(pred: T => Boolean, from: Int, sz: Int) = {
    var i = from
    var cnt = 0
    
    while (i < sz) {
      if (pred(arr(i).asInstanceOf[T])) {
        cnt += 1
        i += 1
      } else i = sz
    }
    
    cnt
  }
  
  def sequentialIndexWhere(pred: T => Boolean, from: Int, sz: Int) = {
    var i = from
    var pos = -1
    
    while (i < sz) {
      if (pred(arr(i).asInstanceOf[T])) {
        pos = i
        i = sz
      } else i += 1
    }
    
    pos
  }
  
  def sequentialLastIndexWhere(pred: T => Boolean, end: Int, sz: Int) = {
    var i = end
    var pos = -1
    
    while (i >= 0) {
      if (pred(arr(i).asInstanceOf[T])) {
        pos = i
        i = -1
      } else i -= 1
    }
    
    pos
  }
  
  def sequentialReverse(sz: Int) = {
    val res = new Array[Any](sz)
    
    var i = sz - 1
    var j = 0
    while (i >= 0) {
      res(j) = arr(i)
      i -= 1
      j += 1
    }
    res
  }
  
  def sequentialReverseMap(f: T => T, sz: Int) = {
    val res = new Array[Any](sz)
    
    var i = sz - 1
    var j = 0
    while (i >= 0) {
      res(j) = f(arr(i).asInstanceOf[T])
      i -= 1
      j += 1
    }
    res
  }
  
  def sequentialSameElements(sq: Seq[T], sz: Int): Boolean = {
    if (sz != sq.length) false
    else {
      var i = 0
      val jt = sq.iterator
      while (i < sz) {
        if (arr(i) == jt.next) i += 1
        else i = sz + 1
      }
      if (i == sz) true
      else false
    }
  }
  
  def sequentialCorresponds(sq: Seq[T], f: (T, T) => Boolean, sz: Int): Boolean = {
    if (sz != sq.length) false
    else {
      var i = 0
      val jt = sq.iterator
      while (i < sz) {
        if (f(arr(i).asInstanceOf[T], jt.next)) i += 1
        else i = sz + 1
      }
      if (i == sz) true
      else false
    }
  }
  
  def sequentialDiff(sq: Seq[T], sz: Int) = {
    val occmap = occurences(sq)
    val b = new collection.mutable.ArrayBuffer[T]
    
    var i = 0
    while (i < sz) {
      val elem = arr(i).asInstanceOf[T]
      if (occmap(elem) == 0) b += elem
      else occmap(elem) -= 1
      i += 1
    }
    
    val res = new Array[Any](b.size)
    b.copyToArray(res, 0)
    res
  }
  
  def sequentialIntersect(sq: Seq[T], sz: Int) = {
    val occmap = occurences(sq)
    val b = new collection.mutable.ArrayBuffer[T]
    
    var i = 0
    while (i < sz) {
      val elem = arr(i).asInstanceOf[T]
      val num = occmap(elem)
      if (num > 0) {
        b += elem
        occmap(elem) = num - 1
      }
      i += 1
    }
    
    val res = new Array[Any](b.size)
    b.copyToArray(res, 0)
    res
  }
  
  private def occurences(sq: Seq[T]) = {
    val occmap = new collection.mutable.HashMap[T, Int] { override def default(k: T) = 0 }
    for (elem <- sq.iterator) occmap(elem) += 1
    occmap
  }
  
  def sequentialRemoveDuplicates(sz: Int) = {
    val occ = new collection.mutable.HashSet[T]
    val b = new collection.mutable.ArrayBuffer[T]
    
    var i = 0
    while (i < sz) {
      val elem = arr(i).asInstanceOf[T]
      if (!occ.contains(elem)) {
        b += elem
        occ.add(elem)
      }
      i += 1
    }
    
    val res = new Array[Any](b.size)
    b.copyToArray(res, 0)
    res
  }
  
  def sequentialPatch(from: Int, p: Seq[T], replaced: Int, sz: Int) = {
    val b = new collection.mutable.ArrayBuffer[T]
    b.sizeHint(from + (sz - from - replaced) + p.size)
    
    var i = 0
    while (i < from) {
      b += arr(i).asInstanceOf[T]
      i += 1
    }
    
    val jt = p.iterator
    while (jt.hasNext) b += jt.next
    
    val skipto = from + replaced
    while (i < from + replaced) i += 1
    
    while (i < sz) {
      b += arr(i).asInstanceOf[T]
      i += 1
    }
    
    val res = new Array[Any](b.size)
    b.copyToArray(res, 0)
    res
  }
  
  def sequentialPadTo(tosize: Int, elem: T, sz: Int) = {
    val b = new collection.mutable.ArrayBuffer[T]
    b.sizeHint(tosize)
    
    var i = 0
    while (i < sz) {
      b += arr(i).asInstanceOf[T]
      i += 1
    }
    
    while (i < tosize) {
      b += elem
      i += 1
    }
    
    val res = new Array[Any](b.size)
    b.copyToArray(res, 0)
    res
  }
  
}



























