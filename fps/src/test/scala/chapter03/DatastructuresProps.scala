package chapter03

import org.scalacheck.*

import chapter03.{List => MyList}

object Generators:
  import Prop.*

  val genList: Gen[MyList[Int]] =
    for {
      size  <- Gen.choose(0, 1000)
      slist <- Gen.listOfN(size, Arbitrary.arbInt.arbitrary)
    } yield slist.foldLeft(List.empty)((l,e) => MyList.append(l,e))

object DatastructuresProps  extends Properties("datastructures"):
  import Generators.*
  import MyList.*

  property("testApply") =
    Prop.forAll((int1: Int, int2: Int, int3: Int) =>
      MyList.apply(int1, int2, int3) == Cons(int1, Cons(int2, Cons(int3, Nil)))
    )

  property("testTailEqualsDropOne") =
    Prop.forAll(genList)((l: MyList[Int]) =>
      tail(l) == drop(l, 1)
    )

  property("testSetHead1") =
    Prop.forAll{ (int1: Int, int2: Int, int3: Int) =>
      setHead(int3, apply(int1, int2, int3)) == apply(int3, int2, int3)
    }

  property("testDrop") =
    Prop.forAll { (int1: Int, int2: Int, int3: Int) =>
      drop(apply(int1, int2, int3),2) == apply(int3)
    }

  property("testInit") =
    Prop.forAll { (int1: Int, int2: Int, int3: Int) =>
      init(apply(int1, int2, int3)) == apply(int1, int2)
    }

  property("testAppend") =
    Prop.forAll { (int1: Int, int2: Int, int3: Int) =>
      append(apply(int1, int2), int3) == apply(int1, int2, int3)
    }
