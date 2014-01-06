



import collection._
import collection.concurrent.TrieMap

import Test.Spec


object SnapshotSpec extends Spec {

  def test() {
    "support snapshots" in {
      val ctn = new TrieMap
      ctn.snapshot()
      ctn.readOnlySnapshot()

      val ct = new TrieMap[Int, Int]
      for (i <- 0 until 100) ct.put(i, i)
      ct.snapshot()
      ct.readOnlySnapshot()
    }

    "empty 2 quiescent snapshots in isolation" in {
      val sz = 4000

      class Worker(trie: TrieMap[Wrap, Int]) extends Thread {
        override def run() {
          for (i <- 0 until sz) {
            assert(trie.remove(new Wrap(i)) == Some(i))
            for (j <- 0 until sz)
              if (j <= i) assert(trie.get(new Wrap(j)) == None)
              else assert(trie.get(new Wrap(j)) == Some(j))
          }
        }
      }

      val ct = new TrieMap[Wrap, Int]
      for (i <- 0 until sz) ct.put(new Wrap(i), i)
      val snapt = ct.snapshot()

      val original = new Worker(ct)
      val snapshot = new Worker(snapt)
      original.start()
      snapshot.start()
      original.join()
      snapshot.join()

      for (i <- 0 until sz) {
        assert(ct.get(new Wrap(i)) == None)
        assert(snapt.get(new Wrap(i)) == None)
      }
    }

    def consistentReadOnly(name: String, readonly: Map[Wrap, Int], sz: Int, N: Int) {
      @volatile var e: Exception = null

      // reads possible entries once and stores them
      // then reads all these N more times to check if the
      // state stayed the same
      class Reader(trie: Map[Wrap, Int]) extends Thread {
        setName("Reader " + name)

        override def run() =
          try check()
          catch {
            case ex: Exception => e = ex
          }

        def check() {
          val initial = mutable.Map[Wrap, Int]()
          for (i <- 0 until sz) trie.get(new Wrap(i)) match {
            case Some(i) => initial.put(new Wrap(i), i)
            case None => // do nothing
          }

          for (k <- 0 until N) {
            for (i <- 0 until sz) {
              val tres = trie.get(new Wrap(i))
              val ires = initial.get(new Wrap(i))
              if (tres != ires) println(i, "initially: " + ires, "traversal %d: %s".format(k, tres))
              assert(tres == ires)
            }
          }
        }
      }

      val reader = new Reader(readonly)
      reader.start()
      reader.join()

      if (e ne null) {
        e.printStackTrace()
        throw e
      }
    }

    // traverses the trie `rep` times and modifies each entry
    class Modifier(trie: TrieMap[Wrap, Int], index: Int, rep: Int, sz: Int) extends Thread {
      setName("Modifier %d".format(index))

      override def run() {
        for (k <- 0 until rep) {
          for (i <- 0 until sz) trie.putIfAbsent(new Wrap(i), i) match {
            case Some(_) => trie.remove(new Wrap(i))
            case None => // do nothing
          }
        }
      }
    }

    // removes all the elements from the trie
    class Remover(trie: TrieMap[Wrap, Int], index: Int, totremovers: Int, sz: Int) extends Thread {
      setName("Remover %d".format(index))

      override def run() {
        for (i <- 0 until sz) trie.remove(new Wrap((i + sz / totremovers * index) % sz))
      }
    }

    "have a consistent quiescent read-only snapshot" in {
      val sz = 10000
      val N = 100
      val W = 10

      val ct = new TrieMap[Wrap, Int]
      for (i <- 0 until sz) ct(new Wrap(i)) = i
      val readonly = ct.readOnlySnapshot()
      val threads = for (i <- 0 until W) yield new Modifier(ct, i, N, sz)

      threads.foreach(_.start())
      consistentReadOnly("qm", readonly, sz, N)
      threads.foreach(_.join())
    }

    // now, we check non-quiescent snapshots, as these permit situations
    // where a thread is caught in the middle of the update when a snapshot is taken

    "have a consistent non-quiescent read-only snapshot, concurrent with removes only" in {
      val sz = 1250
      val W = 100
      val S = 5000

      val ct = new TrieMap[Wrap, Int]
      for (i <- 0 until sz) ct(new Wrap(i)) = i
      val threads = for (i <- 0 until W) yield new Remover(ct, i, W, sz)

      threads.foreach(_.start())
      for (i <- 0 until S) consistentReadOnly("non-qr", ct.readOnlySnapshot(), sz, 5)
      threads.foreach(_.join())
    }

    "have a consistent non-quiescent read-only snapshot, concurrent with modifications" in {
      val sz = 1000
      val N = 7000
      val W = 10
      val S = 7000

      val ct = new TrieMap[Wrap, Int]
      for (i <- 0 until sz) ct(new Wrap(i)) = i
      val threads = for (i <- 0 until W) yield new Modifier(ct, i, N, sz)

      threads.foreach(_.start())
      for (i <- 0 until S) consistentReadOnly("non-qm", ct.readOnlySnapshot(), sz, 5)
      threads.foreach(_.join())
    }

    def consistentNonReadOnly(name: String, trie: TrieMap[Wrap, Int], sz: Int, N: Int) {
      @volatile var e: Exception = null

      // reads possible entries once and stores them
      // then reads all these N more times to check if the
      // state stayed the same
      class Worker extends Thread {
        setName("Worker " + name)

        override def run() =
          try check()
          catch {
            case ex: Exception => e = ex
          }

        def check() {
          val initial = mutable.Map[Wrap, Int]()
          for (i <- 0 until sz) trie.get(new Wrap(i)) match {
            case Some(i) => initial.put(new Wrap(i), i)
            case None => // do nothing
          }

          for (k <- 0 until N) {
            // modify
            for ((key, value) <- initial) {
              val oldv = if (k % 2 == 0) value else -value
              val newv = -oldv
              trie.replace(key, oldv, newv)
            }

            // check
            for (i <- 0 until sz) if (initial.contains(new Wrap(i))) {
              val expected = if (k % 2 == 0) -i else i
              //println(trie.get(new Wrap(i)))
              assert(trie.get(new Wrap(i)) == Some(expected))
            } else {
              assert(trie.get(new Wrap(i)) == None)
            }
          }
        }
      }

      val worker = new Worker
      worker.start()
      worker.join()

      if (e ne null) {
        e.printStackTrace()
        throw e
      }
    }

    "have a consistent non-quiescent snapshot, concurrent with modifications" in {
      val sz = 9000
      val N = 1000
      val W = 10
      val S = 400

      val ct = new TrieMap[Wrap, Int]
      for (i <- 0 until sz) ct(new Wrap(i)) = i
      val threads = for (i <- 0 until W) yield new Modifier(ct, i, N, sz)

      threads.foreach(_.start())
      for (i <- 0 until S) {
        consistentReadOnly("non-qm", ct.snapshot(), sz, 5)
        consistentNonReadOnly("non-qsnap", ct.snapshot(), sz, 5)
      }
      threads.foreach(_.join())
    }

    "work when many concurrent snapshots are taken, concurrent with modifications" in {
      val sz = 12000
      val W = 10
      val S = 10
      val modifytimes = 1200
      val snaptimes = 600
      val ct = new TrieMap[Wrap, Int]
      for (i <- 0 until sz) ct(new Wrap(i)) = i

      class Snapshooter extends Thread {
        setName("Snapshooter")
        override def run() {
          for (k <- 0 until snaptimes) {
            val snap = ct.snapshot()
            for (i <- 0 until sz) snap.remove(new Wrap(i))
            for (i <- 0 until sz) assert(!snap.contains(new Wrap(i)))
          }
        }
      }

      val mods = for (i <- 0 until W) yield new Modifier(ct, i, modifytimes, sz)
      val shooters = for (i <- 0 until S) yield new Snapshooter
      val threads = mods ++ shooters
      threads.foreach(_.start())
      threads.foreach(_.join())
    }

  }

}
