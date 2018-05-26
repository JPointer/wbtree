package com.jps.wbtree

import scala.annotation.tailrec


class WBTreeTest extends org.scalatest.FunSuite {

  def populateTreeRandom(count: Int): WBTreeSet[Int] = {
    @tailrec
    def inner(curTree: WBTreeSet[Int], curCount: Int): WBTreeSet[Int] = {
      if (curCount == 0)
        curTree
      else
        inner(curTree.add(scala.util.Random.nextInt()), curCount - 1)
    }
    inner(WBTreeSetNil, count)
  }
  test("Add and exists test") {
    val set1 = WBTreeSetNil.add(2).add(3).add(4)
    assert(set1.exists(2))
    assert(set1.exists(4))
    assert(!set1.exists(5))
    assert((set1.add(5)).exists(5))
  }
  test("Add remove and exists test") {
    val populatedSet = populateTreeRandom(10000)
    val set1 = populatedSet.add(2).add(32).add(5).remove(5).add(7).remove(2).add(9)
    assert(set1.exists(32))
    assert(!set1.exists(5))
    assert(!set1.exists(2))
  }
  test("Intersect test") {
    val set1 = WBTreeSetNil.add(2).add(3).add(4).add(5).add(6).add(7)
    val set2 = WBTreeSetNil.add(3).add(55).add(7)
    val set3 = set1.intersect(set2)
    assert(set3.exists(3))
    assert(set3.exists(7))
    assert(!set3.exists(55))
    assert(!set3.exists(4))
  }
  test("Sum test") {
    val set1 = WBTreeSetNil.add(1).add(2)
    val set2 = WBTreeSetNil.add(4).add(5)
    val set3 = set1.sum(set2)
    assert(set3.exists(1))
    assert(set3.exists(4))
    assert(set3.exists(5))
    assert(!set3.exists(3))
  }


}
