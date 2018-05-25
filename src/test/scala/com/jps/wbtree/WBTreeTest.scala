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
    val tre = populateTreeRandom(500000)
    val r = scala.util.Random

    wbtree.add(1).add(2).add(3).add(-2)
  }


}
