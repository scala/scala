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

package scala;

import java.util.Arrays;
import java.util.List;

import scala.jdk.javaapi.CollectionConverters;

import org.junit.Test;
import static org.junit.Assert.*;

public class OptionJavaTest {

    private Option<Integer> someIntegerOpt = Option.apply(1);
    private Option<Integer> noneIntegerOpt = Option.apply(null);

    private Option<String> someStringOpt = Option.apply("hi");
    private Option<String> noneStringOpt = Option.apply(null);

    @Test
    public void testIntOptionFunction() {
        Option<Integer> optInt = new scala.collection.StringOps("99")
            .toIntOption()
            .map((o) -> (Integer) o);
        assertEquals(Integer.valueOf(99), optInt.get());
    }

    @Test
    public void testSomeInteger() {
        assertEquals(scala.Some.apply(1), someIntegerOpt);
        assertTrue(someIntegerOpt.isDefined());
        assertFalse(someIntegerOpt.isEmpty());
        assertTrue(someIntegerOpt.nonEmpty());
        assertEquals(1, someIntegerOpt.get().intValue());  // junit4 has no assert(int,int)
    }

    @Test
    public void testNoneInteger() {
        assertEquals(scala.None$.MODULE$, noneIntegerOpt);
        assertEquals(scala.Option$.MODULE$.empty(), noneIntegerOpt);
        assertFalse(noneIntegerOpt.isDefined());
        assertTrue(noneIntegerOpt.isEmpty());
        assertFalse(noneIntegerOpt.nonEmpty());
    }

    @Test(expected = java.util.NoSuchElementException.class)
    public void testGetNoneIntegerFailure() {
        noneIntegerOpt.get();
    }

    @Test
    public void testSomeString() {
        assertEquals(scala.Some.apply("hi"), someStringOpt);
        assertTrue(someStringOpt.isDefined());
        assertFalse(someStringOpt.isEmpty());
        assertTrue(someStringOpt.nonEmpty());
        assertEquals("hi", someStringOpt.get());
    }

    @Test
    public void testNoneString() {
        assertEquals(scala.None$.MODULE$, noneStringOpt);
        assertEquals(scala.Option$.MODULE$.empty(), noneStringOpt);
        assertFalse(noneStringOpt.isDefined());
        assertTrue(noneStringOpt.isEmpty());
        assertFalse(noneStringOpt.nonEmpty());
    }

    @Test(expected = java.util.NoSuchElementException.class)
    public void testGetNoneStringFailure() {
        noneStringOpt.get();
    }

    @Test
    public void testMap() {
        Option<Integer> nextIntegerOpt = Option.apply(2);

        assertEquals(nextIntegerOpt, someIntegerOpt.map((_i) -> 2));
        assertNotEquals(noneIntegerOpt, someIntegerOpt.map((_i) -> null));
        assertEquals(noneIntegerOpt, noneIntegerOpt.map((_i) -> 2));

        Option<String> nextStringOpt = Option.apply("bye");

        assertEquals(nextStringOpt, someStringOpt.map((_s) -> "bye"));
        assertNotEquals(noneStringOpt, someStringOpt.map((_s) -> null));
        assertEquals(noneStringOpt, noneStringOpt.map((_s) -> "bye"));
    }

    @Test
    public void testFlatMap() {
        Option<String> nextStringOpt = Option.apply("bye");

        assertEquals(nextStringOpt, someStringOpt.flatMap((_s) -> nextStringOpt));
        assertEquals(noneStringOpt, someStringOpt.flatMap((_s) -> noneStringOpt));

        assertEquals(noneStringOpt, noneStringOpt.flatMap((_s) -> nextStringOpt));
        assertEquals(noneStringOpt, noneStringOpt.flatMap((_s) -> noneStringOpt));
    }

    @Test
    public void testFilter() {
        assertEquals(someIntegerOpt, someIntegerOpt.filter((_i) -> true));
        assertEquals(noneIntegerOpt, someIntegerOpt.filter((_i) -> false));
        assertEquals(noneIntegerOpt, noneIntegerOpt.filter((_i) -> true));

        assertEquals(someStringOpt, someStringOpt.filter((_s) -> true));
        assertEquals(noneStringOpt, someStringOpt.filter((_s) -> false));
        assertEquals(noneStringOpt, noneStringOpt.filter((_s) -> true));
    }

    @Test
    public void testContains() {
        assertTrue(someIntegerOpt.contains(1));
        assertFalse(noneIntegerOpt.contains(1));

        assertTrue(someStringOpt.contains("hi"));
        assertFalse(noneStringOpt.contains("hi"));
    }

    @Test
    public void testExists() {
        assertTrue(someIntegerOpt.exists((_i) -> true));
        assertFalse(noneIntegerOpt.exists((_i) -> false));

        assertTrue(someStringOpt.exists((_s) -> true));
        assertFalse(noneStringOpt.exists((_s) -> false));
    }

    @Test
    public void testOrElse() {
        Option<Integer> nextIntegerOpt = Option.apply(2);

        assertEquals(someIntegerOpt, someIntegerOpt.orElse(() -> nextIntegerOpt));
        assertEquals(someIntegerOpt, someIntegerOpt.orElse(() -> noneIntegerOpt));
        assertEquals(nextIntegerOpt, noneIntegerOpt.orElse(() -> nextIntegerOpt));
        assertEquals(noneIntegerOpt, noneIntegerOpt.orElse(() -> noneIntegerOpt));

        Option<String> nextStringOpt = Option.apply("bye");

        assertEquals(someStringOpt, someStringOpt.orElse(() -> nextStringOpt));
        assertEquals(someStringOpt, someStringOpt.orElse(() -> noneStringOpt));
        assertEquals(someStringOpt, noneStringOpt.orElse(() -> someStringOpt));
        assertEquals(noneStringOpt, noneStringOpt.orElse(() -> noneStringOpt));
    }

    @Test
    public void testGetOrElse() {
        assertEquals(1, someIntegerOpt.getOrElse(() -> 2).intValue());
        assertEquals(2, noneIntegerOpt.getOrElse(() -> 2).intValue());
        assertEquals(Integer.valueOf(1), someIntegerOpt.getOrElse(() -> null));

        assertEquals("hi", someStringOpt.getOrElse(() -> "bye"));
        assertEquals("bye", noneStringOpt.getOrElse(() -> "bye"));
        assertEquals("hi", someStringOpt.getOrElse(() -> null));

        assertNull(noneIntegerOpt.getOrElse(() -> null));
        assertNull(noneStringOpt.getOrElse(() -> null));
    }

    /* Compiler error: method orNull in class scala.Option[A] cannot
     * be applied to given types;
     *   found: no arguments
     *   required: scala.<:<[scala.runtime.Null,A1]
     *   reason: cannot infer type-variable(s) A1
     * see bug#11625
     */
    // @Test
    // public void testOrNull() {
    //     assertEquals(1, someIntegerOpt.orNull().intValue());
    //     assertNull(noneIntegerOpt.orNull());
    //
    //     assertEquals("hi", someStringOpt.orNull());
    //     assertNull(noneStringOpt.orNull());
    // }

    @Test
    public void testToList() {
        List<Integer> someIntegerList = CollectionConverters.asJava(someIntegerOpt.toList());
        List<Integer> noneIntegerList = CollectionConverters.asJava(noneIntegerOpt.toList());

        assertArrayEquals(new Integer[]{1}, someIntegerList.toArray());
        assertArrayEquals(new Integer[0], noneIntegerList.toArray());

        List<String> someStrList = CollectionConverters.asJava(someStringOpt.toList());
        List<String> noneStrList = CollectionConverters.asJava(noneStringOpt.toList());

        assertArrayEquals(new String[]{"hi"}, someStrList.toArray());
        assertArrayEquals(new String[0], noneStrList.toArray());
    }
}
