#!/bin/bash

mkdir -p target/induction
time build/pack/bin/scalac -Yinduction-heuristics -J-Xss4M -J-Xmx2G -d target/induction test/induction/inductive-implicits-bench.scala
