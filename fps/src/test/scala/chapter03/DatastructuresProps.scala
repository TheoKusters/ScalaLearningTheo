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
    def last[A](l: MyList[A], n: Int): MyList[A] =
      assert(n >= 0)
      assert(n <= size(l))
      if size(l) == n then l else last(tail(l), n)

    Prop.forAll(genList)((l: MyList[Int]) =>
      Prop.forAll(Gen.choose(0, size(l)))(n =>
        drop(l, n) == last(l, size(l) - n)
      )
    )

  property("testInit") =
    Prop.forAll { (int1: Int, int2: Int, int3: Int) =>
      init(apply(int1, int2, int3)) == apply(int1, int2)
    }

  property("testAppend") =
    Prop.forAll { (int1: Int, int2: Int, int3: Int) =>
      append(apply(int1, int2), int3) == apply(int1, int2, int3)
    }
