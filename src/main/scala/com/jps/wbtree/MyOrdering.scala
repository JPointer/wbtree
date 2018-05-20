package com.jps.wbtree

trait MyOrdering[T] {
  def lt(a: T, b: T): Boolean
  def eq(a: T, b: T): Boolean
  def gt(a: T, b: T): Boolean
}

