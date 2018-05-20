package com.jps.wbtree


class WBTreeTest extends org.scalatest.FunSuite {

  test("Adding test") {
    var wbtree: WBTree[Int] = new WBTree[Int]
    wbtree.add(5)
    assert(wbtree.exists(5))
  }
}
