package com.jps.wbtree

import com.jps.wbtree.DataOrdering.MyOrdering

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

object DataOrdering {
  trait MyOrdering[T] {
    def lt(a: T, b: T): Boolean
    def eq(a: T, b: T): Boolean
    def gt(a: T, b: T): Boolean
  }
  implicit object DoubleOrdering extends MyOrdering[Double] {
    override def eq(a: Double, b: Double): Boolean = a == b
    override def lt(a: Double, b: Double): Boolean = a < b
    override def gt(a: Double, b: Double): Boolean = a > b
  }
  implicit object IntOrdering extends MyOrdering[Int] {
    override def eq(a: Int, b: Int): Boolean = a == b
    override def lt(a: Int, b: Int): Boolean = a < b
    override def gt(a: Int, b: Int): Boolean = a > b
  }
  implicit object StringOrdering extends MyOrdering[String] {
    override def eq(a: String, b: String): Boolean = a == b
    override def lt(a: String, b: String): Boolean = a.compareTo(b) < 0
    override def gt(a: String, b: String): Boolean = a.compareTo(b) > 0
  }
  implicit object NothingOrdering extends MyOrdering[Nothing] {
    override def eq(a: Nothing, b: Nothing): Boolean = false
    override def lt(a: Nothing, b: Nothing): Boolean = false
    override def gt(a: Nothing, b: Nothing): Boolean = false
  }
}

sealed abstract class WBTreeSet[+A: MyOrdering] {
  def size: Int = 0
  def iterator[T >: A : MyOrdering]: Iterator[T] = Iterator.empty
  def add[T >: A : MyOrdering](valueAdded: T): WBTreeSet[T] = Node(valueAdded, WBTreeSetNil, WBTreeSetNil)
  def exists[T >: A : MyOrdering](value: T): Boolean = false
  def intersect[T >: A : MyOrdering](other: WBTreeSet[T]): WBTreeSet[T] = WBTreeSetNil
  def sum[T >: A : MyOrdering](other: WBTreeSet[T]): WBTreeSet[T] = other
  def remove[T >: A : MyOrdering](valueRemoved: T): WBTreeSet[T] = throw new NoSuchElementException(String.valueOf(valueRemoved))
  def rebalance[T >: A : MyOrdering]: WBTreeSet[T] = this
  def remove_and_get_min[T >: A : MyOrdering]: (T, WBTreeSet[T]) = throw new IllegalStateException()
  def singleLeftRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
  def doubleLeftRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
  def singleRightRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
  def doubleRightRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
}

case object WBTreeSetNil extends WBTreeSet[Nothing] {
}

case class Node[A: MyOrdering](value: A, left: WBTreeSet[A], right: WBTreeSet[A]) extends WBTreeSet[A] {

  val delta: Double = 1 + Math.sqrt(2)
  val gamma: Double = Math.sqrt(2)

  override val size: Int = left.size + right.size + 1

  override def add[T >: A : MyOrdering](valueAdded: T): WBTreeSet[T] = {
    val comparator = implicitly[MyOrdering[T]]

    if (comparator.lt(valueAdded, value))
      Node(value, left.add(valueAdded), right).rebalance
    else if (comparator.gt(valueAdded, value))
      Node(value, left, right.add(valueAdded)).rebalance
    else
      // Wartość już istnieje, bez zmian
      this
  }

  override def rebalance[T >: A : MyOrdering]: WBTreeSet[T] = {
    if (left.size + right.size < 2)
      this
    else if (right.size > delta * left.size)
      right match {
        case Node(_, rightLeft, rightRight) =>
          if (rightLeft.size + 1 < (rightRight.size + 1) * gamma)
            singleLeftRotate
          else
            doubleLeftRotate
        case _ => throw new IllegalStateException()
      }
    else if (left.size > delta * right.size)
      left match {
        case Node(_, leftLeft, leftRight) =>
          if (leftRight.size + 1 < (leftLeft.size + 1) * gamma)
            singleRightRotate
          else
            doubleRightRotate
        case _ => throw new IllegalStateException()
      }
    else
      this
  }
  override def singleLeftRotate[T >: A : MyOrdering]: Node[T] = {
    this match {
      case Node(curVal, curLeft, Node(curRightVal, curRightLeft, curRightRight))
        => Node(curRightVal, Node(curVal, curLeft, curRightLeft), curRightRight)
        }
  }
  override def doubleLeftRotate[T >: A : MyOrdering]: Node[T] = {
    this match {
      case Node(curVal, curLeft, Node(rightVal, Node(rightLeftVal, rightLeftLeft, rightLeftRight), rightRight))
        => Node[T](rightLeftVal, Node(curVal, curLeft, rightLeftLeft), Node(rightVal, rightLeftRight, rightRight))
    }
  }
  override def singleRightRotate[T >: A : MyOrdering]: Node[T] = {
    this match {
      case Node(curVal, Node(curLeftVal, curLeftLeft, curLeftRight), curRight)
      => Node(curLeftVal, curLeftLeft, Node(curVal, curLeftRight, curRight))
    }
  }
  override def doubleRightRotate[T >: A : MyOrdering]: Node[T] = {
    this match {
      case Node(curVal, Node(leftVal, leftLeft, Node(leftRightVal, leftRightLeft, leftRightRight)), curRight)
        => Node(leftRightVal, Node(leftVal, leftLeft, leftRightLeft), Node(curVal, leftRightRight, curRight))
    }
  }
  override def exists[T >: A : MyOrdering](valueExist: T): Boolean = {
    val comparator = implicitly[MyOrdering[T]]

    if (comparator.eq(valueExist, value))
      true
    else if (comparator.lt(valueExist, value))
      left.exists(valueExist)
    else
      right.exists(valueExist)
  }

/**
  * @param valueToRemove wartośc do usunięcia
  * @return Aktualny węzeł (drzewo) po usunięciu wartości
  */
  override def remove[T >: A : MyOrdering](valueToRemove: T): WBTreeSet[T] = {
    val comparator = implicitly[MyOrdering[T]]

    if (comparator.eq(valueToRemove, value)) {
      // Należy usunąć aktualny węzeł
      left match {
        case WBTreeSetNil =>
          right match {
            // Węzeł na samym dole drzewa, najprostszy przypadek, zwracamy pusty węzeł
            case WBTreeSetNil => WBTreeSetNil
            // Istnieje tylko prawy węzeł-dziecko - zastąpi on aktualnie usuwany węzeł
            case _ => right.rebalance
          }
        case Node(leftVal, leftLeft, leftRight) => {
          right match {
            // istnieje tylko lewy węzeł-dziecko - zastępuje aktualnie usuwany węzeł
            case WBTreeSetNil => left.rebalance
            // istnieje zarówno lewe jak i prawe dziecko - znajdujemy najmniejszy większy element od aktualnego i podmieniamy
            case _ => {
              val (smallestHigher, newRight) = right.remove_and_get_min
              Node(smallestHigher, left, newRight).rebalance
            }
          }
        }
      }
    }
    else if (comparator.lt(valueToRemove, value))
      Node(value, left.remove(valueToRemove), right).rebalance
    else
      Node(value, left, right.remove(valueToRemove)).rebalance
  }
  override def remove_and_get_min[T >: A : MyOrdering]:  (T, WBTreeSet[T]) = {
    left match {
      case WBTreeSetNil => (value, right)
      case _ => {
        val (min, newLeft) = left.remove_and_get_min
        (min, Node(value, newLeft, right).rebalance)
      }
    }
  }

  override def intersect[T >: A : MyOrdering](other: WBTreeSet[T]): WBTreeSet[T] = {
    val comparator = implicitly[MyOrdering[T]]

    val thisIterator = this.iterator
    val otherIterator = other.iterator
    @tailrec
    def inner(set: WBTreeSet[T], curThisIterVal: T, curOtherIterVal: T) : WBTreeSet[T] = {
      if(comparator.eq(curThisIterVal, curOtherIterVal))
        if (thisIterator.hasNext && otherIterator.hasNext)
          inner(set.add(curThisIterVal), thisIterator.next(), otherIterator.next())
        else
          set.add(curThisIterVal)
      else if (comparator.lt(curThisIterVal: T, curOtherIterVal: T))
        if (thisIterator.hasNext) inner(set, thisIterator.next(), curOtherIterVal) else set
      else
      if (otherIterator.hasNext) inner(set, curThisIterVal, otherIterator.next()) else set
    }
    if (!thisIterator.hasNext || !otherIterator.hasNext)
      WBTreeSetNil
    else
      inner(WBTreeSetNil, thisIterator.next(), otherIterator.next())
  }
  override def sum[T >: A : MyOrdering](other: WBTreeSet[T]): WBTreeSet[T] = {
    val otherIterator = other.iterator
    @tailrec
    def inner(set: WBTreeSet[T]) : WBTreeSet[T] = {
      if (otherIterator.hasNext)
        inner(set.add(otherIterator.next))
      else
        set
    }
    inner(this)
  }

  override def iterator[T >: A : MyOrdering]: Iterator[T] = new WBTreeIterator(this)
}

// iteratory w scali są mutowalne
// iterator chodzi w porządku rosnącym
private class WBTreeIterator[T](node: Node[T]) extends Iterator[T] {
  val stack = mutable.ArrayStack[Node[T]]()
  goFarLeft(node)

  def goFarLeft(curNode: WBTreeSet[T]): Unit = {
    curNode match {
      case WBTreeSetNil => Unit
      case node : Node[T] => {
        stack.push(node)
        goFarLeft(node.left)
      }
    }
  }

  override def hasNext: Boolean = stack.nonEmpty

  override def next(): T = {
    val current = stack.pop()
    goFarLeft(current.right)
    current.value
  }
}