package quickcheck

import java.util.NoSuchElementException

import common._

import org.scalacheck._
import math.{min, max}
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == min(a, b)
  }

  property("min3") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(a, insert(b, insert(c, empty)))
    findMin(deleteMin(h)) == List(a, b, c).sorted.drop(1).head
  }

  property("size1") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("delete min 1") = forAll {(a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    deleteMin(h) == insert(max(a, b), empty)
  }

  property("delete min 2") = forAll { l: List[Int] =>
    val sortedl = l.sorted
    val h = sortedl.foldLeft(empty)((accu, curr) => insert(curr, accu))
    genList(h) == sortedl
  }

  property("delete min 3") = forAll { l: List[Int] =>
    if (l.isEmpty) true
    else {
      val sortedl = l.sorted
      val h = sortedl.foldLeft(empty)((accu, curr) => insert(curr, accu))
      sortedl.tail == genList(deleteMin(h))
    }
  }

  property("sorted1") = forAll { h: H =>
    val l = genList(h)
    isSorted(l)
  }

  property("meld sorted") = forAll { (h1: H, h2: H) =>
    val l = genList(meld(h1, h2))
    isSorted(l)
  }

  property("minimum of 2") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    if (isEmpty(h1) && isEmpty(h2)) try {
      findMin(m)
      false
    } catch { case _: NoSuchElementException => true}
    else if (!isEmpty(h1) && isEmpty(h2)) findMin(m) == findMin(h2)
    else if (isEmpty(h1) && !isEmpty(h2)) findMin(m) == findMin(h1)
    else findMin(m) == min(findMin(h1), findMin(h2))
  }

  def genList(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: genList(deleteMin(h))
  }

  def isSorted(l: List[Int]): Boolean = l match {
    case Nil => true
    case x :: xs => if (xs.isEmpty) true else (x <= xs.head) && isSorted(xs)
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
