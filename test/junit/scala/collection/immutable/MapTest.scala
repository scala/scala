package scala.collection.immutable

import org.junit.Assert.assertEquals
import org.junit.Test

class MapTest {

  @Test def builderCompare1: Unit = {
    for (size <- 0 to 100;
         start <- 0 to 10;
         overwrite <- List(true, false)) {
      val tBuilder = TreeMap.newBuilder[String, String]
      val sBuilder = SortedMap.newBuilder[String, String]
      val control  = HashMap.newBuilder[String, String]
      for (i <- start until start + size) {
        sBuilder += i.toString -> "a"
        tBuilder += i.toString -> "a"
        control += i.toString -> "a"
      }
      if (overwrite) {
        for (i <- start until start + size) {
          sBuilder += i.toString -> "b"
          tBuilder += i.toString -> "b"
          control += i.toString -> "b"
        }
      }
      val treeMap  = tBuilder.result()
      val sortMap  = sBuilder.result()
      val expected = control.result()

      assertEquals(expected.size, treeMap.size)
      assertEquals(expected.size, sortMap.size)

      assertEquals(expected, treeMap)
      assertEquals(expected, sortMap)

      assertEquals(expected, treeMap.iterator.toMap)
      assertEquals(expected, sortMap.iterator.toMap)

    }
  }
  @Test def builderCompare2: Unit = {
    for (size <- 0 to 100;
         start <- 0 to 10;
         overwrite <- List(true, false)) {
      val d1       = for (i <- start until start + size) yield {
        i -> "a"
      }
      val d2       = if (overwrite) {
        for (i <- start until start + size) yield {
          i -> "b"
        }
      } else List()
      val data     = d1 ++ d2
      val treeMap  = TreeMap(data: _*)
      val sortMap  = SortedMap(data: _*)
      val expected = HashMap(data: _*)

      assertEquals(expected.size, treeMap.size)
      assertEquals(expected.size, sortMap.size)

      assertEquals(expected, treeMap)
      assertEquals(expected, sortMap)

      assertEquals(expected, treeMap.iterator.toMap)
      assertEquals(expected, sortMap.iterator.toMap)

    }
  }

  @Test def addition: Unit = {
    for (size <- 0 to 100;
         start <- 0 to 10) {
      val tBuilder = TreeMap.newBuilder[String, String]
      val sBuilder = SortedMap.newBuilder[String, String]
      val control  = HashMap.newBuilder[String, String]
      for (i <- start until start + size) {
        sBuilder += i.toString -> "a"
        tBuilder += i.toString -> "a"
        control += i.toString -> "a"
      }

      val treeMap   = tBuilder.result()
      val sortMap   = sBuilder.result()
      val expected1 = control.result()

      assertEquals(expected1.size, treeMap.size)
      assertEquals(expected1.size, sortMap.size)

      assertEquals(expected1, treeMap)
      assertEquals(expected1, sortMap)

      assertEquals(expected1, treeMap.iterator.toMap)
      assertEquals(expected1, sortMap.iterator.toMap)

      val addList  = List.tabulate(size) { i =>
        (i + start).toString -> "b"
      }
      val expected = (expected1 ++ addList).toList.sortBy(_._1)

      val addMap         = addList.toMap
      val addArray       = addList.toArray.toSeq
      val addSortSame    = SortedMap(addList: _*)(Ordering[String])
      val addSortReverse = SortedMap(addList: _*)(Ordering[String].reverse)
      val addTreeSame    = TreeMap(addList: _*)(Ordering[String])
      val addTreeReverse = TreeMap(addList: _*)(Ordering[String].reverse)

      def emptySB = SortedMap.newBuilder[String, String]

      def emptyTB = TreeMap.newBuilder[String, String]

      for (form <- List(addList, addMap, addArray, addSortSame, addSortReverse, addTreeSame, addTreeReverse)) {
        val info = s"form[${form.getClass.getSimpleName}]=$form, size=$size, start=$start"

        assertEquals(info, expected, (treeMap ++ form).toList)
        assertEquals(info, expected, (sortMap ++ form).toList)

        assertEquals(info, expected, (treeMap ++ form.iterator).toList)
        assertEquals(info, expected, (sortMap ++ form.iterator).toList)

        assertEquals(info, expected, (form.foldLeft(treeMap) {
          _ + _
        }).toList)
        assertEquals(info, expected, (form.foldLeft(sortMap) {
          _ + _
        }).toList)

        for (form2 <- List(addList, addMap, addArray, addSortSame, addSortReverse, addTreeSame, addTreeReverse)) {
          val info2 = s"form2[${form2.getClass.getSimpleName}]=$form2, $info"

          assertEquals(info2, expected, (emptySB ++= form ++= form2).result.toList)
          assertEquals(info2, expected, (emptyTB ++= form ++= form2).result.toList)

          assertEquals(info2, expected, ((form.foldLeft(emptySB)(_ += _)) ++= form2).result.toList)
          assertEquals(info2, expected, ((form.foldLeft(emptyTB)(_ += _)) ++= form2).result.toList)

          assertEquals(info2, expected, (form2.foldLeft(form.foldLeft(emptySB)(_ += _))(_ += _)).result.toList)
          assertEquals(info2, expected, (form2.foldLeft(form.foldLeft(emptySB)(_ += _))(_ += _)).result.toList)

        }
      }
    }
  }
}
