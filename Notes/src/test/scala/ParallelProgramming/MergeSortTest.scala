package ParallelProgramming

import org.scalatest.{FunSuite, Matchers}

class MergeSortTest extends FunSuite with Matchers {

  test("MergeSort works with different depth values") {

    val in: Array[Int] = Array(1, -6, 8, 0, 10, -19)

    testForDepths(in, 10) shouldBe true

    def testForDepths(as: Array[Int], depth: Int): Boolean = {
      if (depth >= 0) {
        MergeSort.parallelMergeSort(as, depth)
        verifyIsSorted(in) && testForDepths(scala.util.Random.shuffle(in.toList).toArray, depth - 1)
      } else true
    }
  }

  private def verifyIsSorted(as: Array[Int]): Boolean = {
    as.grouped(2).forall(ar => if (ar.length == 2) ar(1) >= ar(0) else true)
  }

}
