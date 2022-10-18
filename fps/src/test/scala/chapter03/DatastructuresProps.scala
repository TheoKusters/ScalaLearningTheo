package chapter03

import org.scalacheck.*

import chapter03.{List => MyList}

object Generators:
  import Prop.*

  val int1: Gen[Int] = Gen.choose(0, 100)
  val int2: Gen[Int] = Gen.choose(0, 100)
  val int3: Gen[Int] = Gen.choose(0, 100)

  val genList: Gen[MyList[Int]] =
    for {
      size  <- Gen.choose(0, 1000)
      slist <- Gen.listOfN(size, Arbitrary.arbInt.arbitrary)
    } yield slist.foldLeft(List.empty)((l,e) => MyList.append(l,e))

  val genSingular: Gen[Int] = Gen.choose[Int](-1000, 1000)

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
    Prop.forAll(genList)((l: MyList[Int]) =>
      tail(setHead(genSingular, l)) == tail(l)
    )

  property("testSetHead2") =
    Prop.forAll(genList)((l: MyList[Int]) =>
      setHead(genSingular, l) == Cons(genSingular, tail(l))
    )

  property("testDrop") =
    Prop.forAll(genList)((l: MyList[Int]) =>
      val maxDrop = Gen.choose(0, size(l))
      def go(l: MyList[Int], droppedList: MyList[Int], n: Int): MyList[Int] =
        if (maxDrop > n)
          l match
            case (h, _) => append(droppedList, h)
          go(tail(l), droppedList, n + 1)
        else droppedList
      drop(l, maxDrop) == go(l, MyList.empty, Gen.choose(0, size(l)))
    )

  property("testInit") =
    Prop.forAll { (int1: Int, int2: Int, int3: Int) =>
      init(apply(int1, int2, int3)) == apply(int1, int2)
    }

  property("testAppend") =
    Prop.forAll { (int1: Int, int2: Int, int3: Int) =>
      append(apply(int1, int2), int3) == apply(int1, int2, int3)
    }
