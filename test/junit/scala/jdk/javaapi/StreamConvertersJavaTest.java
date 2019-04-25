/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.jdk.javaapi;

import org.junit.Test;
import scala.Tuple2;
import scala.collection.Map;
import scala.collection.immutable.List;

import java.util.Arrays;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.junit.Assert.assertEquals;

public class StreamConvertersJavaTest {
    public List<String> ls = CollectionConverters.asScala(Arrays.asList("a", "b")).toList();
    public List<Integer> li = CollectionConverters.asScala(Arrays.asList(1, 2)).toList();
    public List<Float> lf = CollectionConverters.asScala(Arrays.asList(1f, 2f)).toList();

    public <K, V> Map<K, V> mkm(K k1, V v1, K k2, V v2) {
        java.util.Map<K, V> m = new java.util.HashMap<K, V>();
        m.put(k1, v1);
        m.put(k2, v2);
        return CollectionConverters.asScala(m);
    }
    public Map<String, String> mss = mkm("a", "aa", "b", "bb");
    public Map<Float, Integer> mfi = mkm(1f, 1, 2f, 2);

    @Test
    public void convertersTest() {
        Stream<String> lss = StreamConverters.asJavaSeqStream(ls);
        assertEquals("ab", lss.reduce("", (x, y) -> x + y));

        Stream<Integer> lis = StreamConverters.asJavaParStream(li);
        assertEquals(3, lis.reduce(0, Integer::sum).intValue());

        IntStream liis = StreamConverters.asJavaSeqIntStream(li);
        assertEquals(3, liis.sum());

        DoubleStream lfds = StreamConverters.asJavaParDoubleStreamFromFloat(lf);
        assertEquals(3d, lfds.sum(), 0.1);
    }

    @Test
    public void mapConvertersTest() {
        Stream<Tuple2<String, String>> msss = StreamConverters.asJavaSeqStream(mss);
        assertEquals("aaabbb", msss.reduce("", (x, y) -> x + y._1 + y._2, (x, y) -> x + y));

        Stream<String> mssks = StreamConverters.asJavaParKeyStream(mss);
        assertEquals("ab", mssks.reduce("", (x, y) -> x + y));

        Stream<String> mssvs = StreamConverters.asJavaSeqValueStream(mss);
        assertEquals("aabb", mssvs.reduce("", (x, y) -> x + y));

        Stream<Tuple2<Float, Integer>> mfis = StreamConverters.asJavaParStream(mfi);
        assertEquals(6.0, mfis.reduce(0f, (x, y) -> x + y._1 + y._2, Float::sum), 0.1);

        Stream<Float> mfiks = StreamConverters.asJavaSeqKeyStream(mfi);
        assertEquals(3.0, mfiks.reduce(0f, Float::sum), 0.1);

        Stream<Integer> mfivs = StreamConverters.asJavaParValueStream(mfi);
        assertEquals(3, mfivs.reduce(0, Integer::sum).intValue());

        DoubleStream mfikds = StreamConverters.asJavaParKeyDoubleStreamFromFloat(mfi);
        assertEquals(3.0, mfikds.sum(), 0.1);

        IntStream mfivis = StreamConverters.asJavaSeqValueIntStream(mfi);
        assertEquals(3, mfivis.sum());
    }
}
