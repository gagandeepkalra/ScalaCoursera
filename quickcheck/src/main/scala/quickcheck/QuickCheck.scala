package quickcheck

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, _}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- Arbitrary.arbitrary[Int]
    m <- Gen.oneOf[H](Gen.const[H](empty), genHeap)
  } yield insert(n, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insertSingleElementBeEqualToMinAndDeleteBeEmpty") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a && isEmpty(deleteMin(h))
  }

  property("insertTwoElementsMinBeMinOfThTwo") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == math.min(a, b)
  }

  property("minOfTwoHeapsBeEqualToMinOfMerge") = forAll { (h1: H, h2: H) =>

    val min1 = findMin(h1)
    val min2 = findMin(h2)
    findMin(meld(h1, h2)) == math.min(min1, min2)

  }

  property("extractingMinRepeatedlyGivesSortedSequence") = forAll { (h: H) =>
    def verifyAllElementsGreaterThanLastMin(h: H, lastMin: Int): Boolean = {
      if (isEmpty(h)) true
      else if (findMin(h) >= lastMin) {
        verifyAllElementsGreaterThanLastMin(deleteMin(h), findMin(h))
      }
      else false
    }

    verifyAllElementsGreaterThanLastMin(deleteMin(h), findMin(h))
  }

  property("mergeContainsAllElementsFromBothLists") = forAll { (h1: H, h2: H) =>
    def sortedList(h: H): List[Int] = {
      if (isEmpty(h)) Nil else findMin(h) :: sortedList(deleteMin(h))
    }

    def mergeSortedLists(l1: List[Int], l2: List[Int]): List[Int] = {
      (l1, l2) match {
        case (x, Nil) => x
        case (Nil, x) => x
        case (l11 :: ls1, l22 :: ls2) => if (l11 <= l22) l11 :: mergeSortedLists(ls1, l2) else l22 :: mergeSortedLists(l1, ls2)
      }
    }

    val l1 = sortedList(h1)
    val l2 = sortedList(h2)
    val l3 = sortedList(meld(h1, h2))

    mergeSortedLists(l1, l2) == l3
  }

}
