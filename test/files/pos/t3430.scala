// package com.example

object A {    
  def f1(f: String => Boolean) = f("a")

  def f2(): Boolean =
    f1 { s1 =>
      f1 { s2 =>
        while (true) { }
        true
      }
    }
}