package ParallelProgramming

import ParallelProgramming.common.common.parallel

object MergeSort {

  def parallelMergeSort(xs: Array[Int], maxDepth: Int): Unit = {
    val ys = new Array[Int](xs.length)

    sort(0, xs.length, 0)
    if (maxDepth % 2 != 0) copy(ys, xs, 0, xs.length, 0)

    def sort(from: Int, until: Int, depth: Int): Unit = {
      if (from < until)
        if (depth == maxDepth) {
          java.util.Arrays.sort(xs, from, until)
        } else {
          val mid = (from + until) / 2
          parallel(sort(from, mid, depth + 1), sort(mid, until, depth + 1))

          val flip = (maxDepth - depth) % 2 == 0
          val src = if (!flip) xs else ys
          val dst = if (flip) xs else ys

          merge(src, dst, from, mid, until)
        }
    }

    def copy(src: Array[Int], dst: Array[Int], from: Int, until: Int, depth: Int): Unit = {
      if (depth == maxDepth) {
        Array.copy(src, from, dst, from, until - from)
      } else {
        val mid = (from + until) / 2
        parallel(copy(src, dst, from, mid, depth + 1), copy(src, dst, mid, until, depth + 1))
      }
    }

    def merge(src: Array[Int], dst: Array[Int], from: Int, mid: Int, until: Int): Unit = {
      var k = from
      var i = from
      var j = mid

      while (i < mid && j < until) {
        if (src(i) <= src(j)) {
          dst(k) = src(i)
          i += 1
          k += 1
        } else {
          dst(k) = src(j)
          j += 1
          k += 1
        }
      }

      while (i < mid) {
        dst(k) = src(i)
        k += 1
        i += 1
      }

      while (j < until) {
        dst(k) = src(j)
        k += 1
        j += 1
      }
    }
  }

  def quickSort(ls: List[Int]): List[Int] = {
    ls match {
      case List() => ls
      case head :: tail => {
        val (first, second) = tail.partition(_ < head)
        quickSort(first) ::: (head :: quickSort(second))
      }
    }
  }

}
