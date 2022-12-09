package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for
      v <- arbitrary[A]
      h <- genHeap
    yield insert(v, h)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (n: Int) =>
    findMin(insert(n, empty)) == n
  }

  property("findMin") = forAll { (h: H) =>
    findMin(insert(Int.MaxValue, h)) == findMin(deleteMin(insert(Int.MinValue, insert(Int.MaxValue, h))))
  }

  property("deleteMin") = forAll { (h: H) =>
    findMin(deleteMin(insert(Int.MinValue, insert(Int.MaxValue, h)))) == findMin(deleteMin(insert(Int.MaxValue, insert(Int.MinValue, h))))
  }

  property("meld") = forAll { (rh1: H, rh2: H) =>
    val h1 = insert(Int.MaxValue, rh1)
    val h2 = insert(Int.MaxValue, rh2)
    val m = meld(h1, h2)
    findMin(h1) >= findMin(m) && findMin(h2) >= findMin(m)
  }
