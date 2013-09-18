


import collection.concurrent.TrieMap
import Test.Spec


object ConcurrentMapSpec extends Spec {

  val initsz = 500
  val secondsz = 750

  def test() {
    "support put" in {
      val ct = new TrieMap[Wrap, Int]
      for (i <- 0 until initsz) assert(ct.put(new Wrap(i), i) == None)
      for (i <- 0 until initsz) assert(ct.put(new Wrap(i), -i) == Some(i))
    }

    "support put if absent" in {
      val ct = new TrieMap[Wrap, Int]
      for (i <- 0 until initsz) ct.update(new Wrap(i), i)
      for (i <- 0 until initsz) assert(ct.putIfAbsent(new Wrap(i), -i) == Some(i))
      for (i <- 0 until initsz) assert(ct.putIfAbsent(new Wrap(i), -i) == Some(i))
      for (i <- initsz until secondsz) assert(ct.putIfAbsent(new Wrap(i), -i) == None)
      for (i <- initsz until secondsz) assert(ct.putIfAbsent(new Wrap(i), i) == Some(-i))
    }

    "support remove if mapped to a specific value" in {
      val ct = new TrieMap[Wrap, Int]
      for (i <- 0 until initsz) ct.update(new Wrap(i), i)
      for (i <- 0 until initsz) assert(ct.remove(new Wrap(i), -i - 1) == false)
      for (i <- 0 until initsz) assert(ct.remove(new Wrap(i), i) == true)
      for (i <- 0 until initsz) assert(ct.remove(new Wrap(i), i) == false)
    }

    "support replace if mapped to a specific value" in {
      val ct = new TrieMap[Wrap, Int]
      for (i <- 0 until initsz) ct.update(new Wrap(i), i)
      for (i <- 0 until initsz) assert(ct.replace(new Wrap(i), -i - 1, -i - 2) == false)
      for (i <- 0 until initsz) assert(ct.replace(new Wrap(i), i, -i - 2) == true)
      for (i <- 0 until initsz) assert(ct.replace(new Wrap(i), i, -i - 2) == false)
      for (i <- initsz until secondsz) assert(ct.replace(new Wrap(i), i, 0) == false)
    }

    "support replace if present" in {
      val ct = new TrieMap[Wrap, Int]
      for (i <- 0 until initsz) ct.update(new Wrap(i), i)
      for (i <- 0 until initsz) assert(ct.replace(new Wrap(i), -i) == Some(i))
      for (i <- 0 until initsz) assert(ct.replace(new Wrap(i), i) == Some(-i))
      for (i <- initsz until secondsz) assert(ct.replace(new Wrap(i), i) == None)
    }

    def assertEqual(a: Any, b: Any) = {
      if (a != b) println(a, b)
      assert(a == b)
    }

    "support replace if mapped to a specific value, using several threads" in {
      val ct = new TrieMap[Wrap, Int]
      val sz = 55000
      for (i <- 0 until sz) ct.update(new Wrap(i), i)

      class Updater(index: Int, offs: Int) extends Thread {
        override def run() {
          var repeats = 0
          for (i <- 0 until sz) {
            val j = (offs + i) % sz
            var k = Int.MaxValue
            do {
              if (k != Int.MaxValue) repeats += 1
              k = ct.lookup(new Wrap(j))
            } while (!ct.replace(new Wrap(j), k, -k))
          }
          //println("Thread %d repeats: %d".format(index, repeats))
        }
      }

      val threads = for (i <- 0 until 16) yield new Updater(i, sz / 32 * i)
      threads.foreach(_.start())
      threads.foreach(_.join())

      for (i <- 0 until sz) assertEqual(ct(new Wrap(i)), i)

      val threads2 = for (i <- 0 until 15) yield new Updater(i, sz / 32 * i)
      threads2.foreach(_.start())
      threads2.foreach(_.join())

      for (i <- 0 until sz) assertEqual(ct(new Wrap(i)), -i)
    }

    "support put if absent, several threads" in {
      val ct = new TrieMap[Wrap, Int]
      val sz = 110000

      class Updater(offs: Int) extends Thread {
        override def run() {
          for (i <- 0 until sz) {
            val j = (offs + i) % sz
            ct.putIfAbsent(new Wrap(j), j)
            assert(ct.lookup(new Wrap(j)) == j)
          }
        }
      }

      val threads = for (i <- 0 until 16) yield new Updater(sz / 32 * i)
      threads.foreach(_.start())
      threads.foreach(_.join())

      for (i <- 0 until sz) assert(ct(new Wrap(i)) == i)
    }

    "support remove if mapped to a specific value, several threads" in {
      val ct = new TrieMap[Wrap, Int]
      val sz = 55000
      for (i <- 0 until sz) ct.update(new Wrap(i), i)

      class Remover(offs: Int) extends Thread {
        override def run() {
          for (i <- 0 until sz) {
            val j = (offs + i) % sz
            ct.remove(new Wrap(j), j)
            assert(ct.get(new Wrap(j)) == None)
          }
        }
      }

      val threads = for (i <- 0 until 16) yield new Remover(sz / 32 * i)
      threads.foreach(_.start())
      threads.foreach(_.join())

      for (i <- 0 until sz) assert(ct.get(new Wrap(i)) == None)
    }

    "have all or none of the elements depending on the oddity" in {
      val ct = new TrieMap[Wrap, Int]
      val sz = 65000
      for (i <- 0 until sz) ct(new Wrap(i)) = i

      class Modifier(index: Int, offs: Int) extends Thread {
        override def run() {
          for (j <- 0 until sz) {
            val i = (offs + j) % sz
            var success = false
            do {
              if (ct.contains(new Wrap(i))) {
                success = ct.remove(new Wrap(i)) != None
              } else {
                success = ct.putIfAbsent(new Wrap(i), i) == None
              }
            } while (!success)
          }
        }
      }

      def modify(n: Int) = {
        val threads = for (i <- 0 until n) yield new Modifier(i, sz / n * i)
        threads.foreach(_.start())
        threads.foreach(_.join())
      }

      modify(16)
      for (i <- 0 until sz) assertEqual(ct.get(new Wrap(i)), Some(i))
      modify(15)
      for (i <- 0 until sz) assertEqual(ct.get(new Wrap(i)), None)
    }

    "compute size correctly" in {
      val ct = new TrieMap[Wrap, Int]
      val sz = 36450
      for (i <- 0 until sz) ct(new Wrap(i)) = i

      assertEqual(ct.size, sz)
      assertEqual(ct.size, sz)
    }

    "compute size correctly in parallel" in {
      val ct = new TrieMap[Wrap, Int]
      val sz = 36450
      for (i <- 0 until sz) ct(new Wrap(i)) = i
      val pct = ct.par

      assertEqual(pct.size, sz)
      assertEqual(pct.size, sz)
    }

  }

}
