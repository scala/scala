package com.netgents.hello;

public class Outer<A> {

  public Inner inner = new Inner();

  public class Inner {

    public A a;
  }
}
