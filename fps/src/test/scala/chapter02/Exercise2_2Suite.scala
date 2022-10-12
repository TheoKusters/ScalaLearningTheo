package chapter02

import IsSorted.*
import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

object Exercise2_2Props  extends Properties("sorted") {

  val genIntArray = Gen.containerOf[Array, Int](
    Gen.chooseNum(Int.MinValue, 100)
  )
  val smallInteger = Gen.choose(0,100)
  val bigInteger = Gen.choose(1000,2000)
  val genIntArraySorted = genIntArray.map(_.sorted)

  property("notSortedArray") = forAll(genIntArray, smallInteger, bigInteger) {(array,smallInt,bigInt) => !isSorted(array :+ bigInt :+ smallInt, (a: Int, b: Int) => b >= a ) }
  property("SortedArray") = forAll(genIntArraySorted) {array => isSorted(array, (a: Int, b: Int) => b >= a ) }

}
