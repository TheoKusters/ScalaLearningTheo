package chapter03

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll
import List.*

val int1 = Gen.choose(0, 100)
val int2 = Gen.choose(0, 100)
val int3 = Gen.choose(0, 100)
//val genIntList = Gen.containerOf[List: Int] (
//  Gen.chooseNum(Int.MinValue, Int.MaxValue)
//)

object DatastructuresProps  extends Properties("datastructures") {

  property("testApply") =
    forAll(int1, int2, int3) { (int1, int2, int3) =>
      apply(int1, int2, int3) == Cons(int1, (Cons(int2, Cons(int3, Nil))))
    }

  property("testTail") =
    forAll(int1, int2, int3) { (int1, int2, int3) =>
      tail(apply(int1, int2, int3)) == apply(int2, int3)
    }

//  property("testTail2") =
//    forAll(genIntList) { (genIntList) =>
//      tail(genIntList) ==
//    }

  property("testSetHead1") =
    forAll(int1, int2, int3) { (int1, int2, int3) =>
      setHead(int3, apply(int1, int2, int3)) == apply(int3, int2, int3)
    }

//  property("testSetHead2") = forAll(int1, int2, int3) { (int1, int2, int3) => setHead(Nil, apply(int1 int2 int3)) == apply(Nil) }

  property("testDrop") =
    forAll(int1, int2, int3) { (int1, int2, int3) =>
      drop(apply(int1, int2, int3),2) == apply(int3)
    }

//  property("testDropWhile") =
//    forAll(int1, int2, int3) { (int1, int2, int3) =>
//      dropWhile(apply(int1, int2, int3), x => x < 3) == apply(int2, int3)
//    }

  property("testInit") =
    forAll(int1, int2, int3) { (int1, int2, int3) =>
      init(apply(int1, int2, int3)) == apply(int1, int2)
    }

  property("testAppend") =
    forAll(int1, int2, int3) { (int1, int2, int3) =>
      append(apply(int1, int2), int3) == apply(int1, int2, int3)
    }

}
