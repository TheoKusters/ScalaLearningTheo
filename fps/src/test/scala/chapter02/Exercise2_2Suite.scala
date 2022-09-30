package chapter02

import IsSorted.*

class Exercise2_2Suite {

  assert(isSorted(Array(1,2,1,4,5), (a: Int, b: Int)=> b >= a), "Array sortedArray is not sorted")
  assert(!isSorted(Array(5,4,3,2,1), (a: Int, b: Int)=> b >= a), "Array unsortedArray is sorted")

}
