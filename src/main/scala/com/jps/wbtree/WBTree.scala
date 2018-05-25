package com.jps.wbtree

import com.jps.wbtree.DataOrdering.MyOrdering

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

class Set[A: MyOrdering] extends WBTree[A] {
  override def add[T >: A : MyOrdering](valueAdded: T): WBTree[T] = super.add(valueAdded)
  override def exists[T >: A : MyOrdering](value: T): Boolean = super.exists(value)
  override def remove[T >: A : MyOrdering](valueRemoved: T): WBTree[T] = super.remove(valueRemoved)
}

private[wbtree] sealed abstract class WBTree[+A: MyOrdering] {
  def weight: Int = 0
  def iterator[T >: A : MyOrdering]: Iterator[T] = Iterator.empty
  def add[T >: A : MyOrdering](valueAdded: T): WBTree[T] = Node(valueAdded, WBTreeNil, WBTreeNil)
  def exists[T >: A : MyOrdering](value: T): Boolean = false
  def remove[T >: A : MyOrdering](valueRemoved: T): WBTree[T] = throw new NoSuchElementException(String.valueOf(valueRemoved))
  protected def rebalance[T >: A : MyOrdering]: WBTree[T] = this
  protected def remove_and_get_min[T >: A : MyOrdering]: (T, WBTree[T]) = throw new IllegalStateException()
  protected def singleLeftRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
  protected def doubleLeftRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
  protected def singleRightRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
  protected def doubleRightRotate[T >: A : MyOrdering]: Node[T] = throw new IllegalStateException()
}

private case object WBTreeNil extends WBTree[Nothing] {
}

private case class Node[A: MyOrdering](value: A, left: WBTree[A], right: WBTree[A]) extends WBTree[A] {

  val delta: Double = 1 + Math.sqrt(2)
  val gamma: Double = Math.sqrt(2)

  override val weight: Int = left.weight + right.weight + 1

  override def add[T >: A : MyOrdering](valueAdded: T): WBTree[T] = {
    val comparator = implicitly[MyOrdering[T]]

    if (comparator.lt(valueAdded, value))
      Node(value, left.add(valueAdded), right).rebalance
    else if (comparator.gt(valueAdded, value))
      Node(value, left, right.add(valueAdded)).rebalance
    else
      // Wartość już istnieje, bez zmian
      this
  }

  override def rebalance[T >: A : MyOrdering]: WBTree[T] = {
    if (left.weight + right.weight < 2)
      this
    else if (right.weight > delta * left.weight)
      right match {
        case Node(_, rightLeft, rightRight) =>
          if (rightLeft.weight < rightRight.weight * gamma)
            singleLeftRotate
          else
            doubleLeftRotate
        case _ => throw new IllegalStateException()
      }
    else if (left.weight > delta * right.weight)
      left match {
        case Node(_, leftLeft, leftRight) =>
          if (leftRight.weight < leftLeft.weight * gamma)
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
  override def remove[T >: A : MyOrdering](valueToRemove: T): WBTree[T] = {
    val comparator = implicitly[MyOrdering[T]]

    if (comparator.eq(valueToRemove, value)) {
      // Należy usunąć aktualny węzeł
      left match {
        case WBTreeNil =>
          right match {
            // Węzeł na samym dole drzewa, najprostszy przypadek, zwracamy pusty węzeł
            case WBTreeNil => WBTreeNil
            // Istnieje tylko prawy węzeł-dziecko - zastąpi on aktualnie usuwany węzeł
            case _ => right.rebalance
          }
        case Node(leftVal, leftLeft, leftRight) => {
          right match {
            // istnieje tylko lewy węzeł-dziecko - zastępuje aktualnie usuwany węzeł
            case WBTreeNil => left.rebalance
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
  override def remove_and_get_min[T >: A : MyOrdering]:  (T, WBTree[T]) = {
    left match {
      case WBTreeNil => (value, right)
      case _ => {
        val (min, newLeft) = left.remove_and_get_min
        (min, Node(value, newLeft, right).rebalance)
      }
    }
  }

  override def iterator[T >: A : MyOrdering]: Iterator[T] = new WBTreeIterator(this)
}

// iteratory w scali są mutowalne
// iterator chodzi w porządku rosnącym
private class WBTreeIterator[T](node: Node[T]) extends Iterator[T] {
  val stack = mutable.ArrayStack[Node[T]]()
  goFarLeft(node)

  def goFarLeft(curNode: WBTree[T]): Unit = {
    curNode match {
      case WBTreeNil => Unit
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