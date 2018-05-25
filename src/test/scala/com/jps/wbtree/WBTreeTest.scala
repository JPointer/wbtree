package com.jps.wbtree

import scala.annotation.tailrec


class WBTreeTest extends org.scalatest.FunSuite {

  def populateTreeRandom(count: Int): WBTree[Int] = {
    @tailrec
    def inner(curTree: WBTree[Int], curCount: Int): WBTree[Int] = {
      if (curCount == 0)
        curTree
      else
        inner(curTree.add(scala.util.Random.nextInt()), curCount - 1)
    }
    inner(WBTreeNil, count)
  }
  test("Adding test") {
    val wbtree: WBTree[Int] = WBTreeNil
    val tre = populateTreeRandom(500)
    val r = scala.util.Random
    for (x <- tre.iterator) {
      println(x)
    }
//
//    val test = wbtree.add(1).add(2).add(33).add(-2).remove(-2)
//    val test2 = test
//      .add(2)
//      .add(5)
//      .add(4)
//      .add(2)
//      .remove(33)
//      .add(2)
//      .add(43)
//    val test3 = test2
  }


}
