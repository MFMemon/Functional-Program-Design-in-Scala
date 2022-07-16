package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:

  lazy val genHeap: Gen[H] =
    for
      t <- oneOf(const(empty), nonEmptyHeap)
    yield
      t
  
  def nonEmptyHeap = 
    for
     i <- arbitrary[A]
     j <- genHeap
    yield
     insert(i, j)

     

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minInEmpty") = forAll{(n :Int) =>
    val h = insert(n, empty)
    findMin(h) == n
  }

  property("deleteMin") = forAll{(n :Int) =>
    val h = insert(n, empty)
    deleteMin(h) == empty
  }

  property("minOfTwo") = forAll{(a: Int, b: Int) =>
    val h = insert(b,insert(a, empty))
    val minOfTwo = if a < b then a else b
    findMin(h) == minOfTwo
  }

  property("minOfMeld") = forAll{(h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val minOfMeld = findMin(meld(h1, h2)) 
    minOfMeld == min1 || minOfMeld == min2
  }

  property("sortedMin") = forAll{ (h: H) =>

    def findMinRecur(h: H): Boolean = 
      val delMin = deleteMin(h)
      if isEmpty(delMin) then
        true
      else
        val res = findMin(h) < findMin(delMin)
        findMinRecur(delMin) && res

    findMinRecur(h)
  }
  // from(0)
  // 0 #:: from(1)
  // 0 #:: {1 #:: from(2)}
  // 0 #:: 1 #:: 2 #:: from(3)
  // 0 #:: 1 #:: 2 #:: 3 #:: from(4)
  // 0 #:: 1 #:: 2 #:: 3 #:: 4 #:: from(5)


/* Derivation of oneOf method use in genHeap.
  oneOf(const(empty), nonEmptyHeap).map(x => x).sample
  vectorofseq = Seq(const(empty), nonEmptyHeap)
  {choose(0,1).map(y => vectorofseq(y))}.map(x => x).sample
  {Choose.xmap[Long, Int](a => a.toInt, b => b.toLong)
    .choose(0,1)}
      .map(y => vectorofseq(y))
        .map(x => x).sample
  {chooseLong.choose(0.toLong, 1.toLong).map(a => a.toInt)}
    .map(y => vectorofseq(y))
      .map(x => x).sample
  {gen(chLng(0,1)).map(a => a.toInt)}
    .map(y => vectorofseq(y))
      .map(x => x).sample
  gen{(p, seed) => doApply(p, seed).map(a => a.toInt)}
    .map(y => vectorofseq(y))
      .map(x => x)
        .sample
  gen{(p, seed) => p.useInitialSeed(seed)(chLng(0,1)).map(a => a.toInt)}
    .map(y => vectorofseq(y))
      .map(x => x)
        .sample

  //let's consider p = Gen.Parameters.Default. So initialSeed = None. This will be more clear when sample method will be called in the end.

  gen{(p, seed) => chLng(0,1)(p,seed).map(a => a.toInt)}
    .map(y => vectorofseq(y))
      .map(x => x)
        .sample
  gen{(p, seed) => r(Some(0 + (seed.d & 0x7fffffffffffffffL) % 2), seed.next).map(a => a.toInt)}
    .map(y => vectorofseq(y))
      .map(x => x)
        .sample
  gen{(p, seed) => r(Some((0 + (seed.d & 0x7fffffffffffffffL) % 2).toInt), seed.next)}
    .map(y => vectorofseq(y))
      .map(x => x)
        .sample
  gen{(p,seed) => doApply(p,seed).map(y => vectorofseq(y))}
    .map(x => x)
      .sample
  gen{(p,seed) => p.useInitialSeed(seed)( (p, seed) => r(Some((0 + (seed.d & 0x7fffffffffffffffL) % 2).toInt), seed.next) ).map(y => vectorofseq(y))}
    .map(x => x)
      .sample  

  //let's consider p = Gen.Parameters.Default. So initialSeed = None. This will be more clear when sample method will be called in the end.

  gen{(p,seed) => r(Some((0 + (seed.d & 0x7fffffffffffffffL) % 2).toInt), seed.next).map(y => vectorofseq(y))}
    .map(x => x)
      .sample  
  gen{(p,seed) => r(Some(vectorofseq((0 + (seed.d & 0x7fffffffffffffffL) % 2).toInt)), seed.next)}
    .map(x => x)
      .sample  
  gen{(p,seed) => doApply(p,seed).map(x => x)}
    .sample
  gen{(p,seed) => p.useInitialSeed(seed)((p,seed) => r(Some(vectorofseq((0 + (seed.d & 0x7fffffffffffffffL) % 2).toInt)), seed.next)).map(x => x)}
    .sample  
    
  //let's consider p = Gen.Parameters.Default. So initialSeed = None. This will be more clear when sample method will be called in the end.

  gen{(p,seed) => r(Some(vectorofseq((0 + (seed.d & 0x7fffffffffffffffL) % 2).toInt)), seed.next).map(x => x)}
    .sample  
  gen{(p,seed) => r(Some(vectorofseq((0 + (seed.d & 0x7fffffffffffffffL) % 2).toInt)), seed.next)}
    .sample
  gen{(p,seed) => r(Some(vectorofseq((0 + (seed.d & 0x7fffffffffffffffL) % 2).toInt)), seed.next)}
    .doApply(Gen.Parameters.default, Seed.random()).retrieve  
  Gen.Parameters.default.useInitialSeed(Seed.random())((p,seed) => r(Some(vectorofseq((0 + (seed.d & 0x7fffffffffffffffL) % 2).toInt)), seed.next)).retrieve
  {(Gen.Parameters.default,Seed.random()) => r(Some(vectorofseq((0 + (seed.d & 0x7fffffffffffffffL) % 2).toInt)), seed.next)}.retrieve

  //lets assume value of 'd' is initialized to 9223372036854775807 based on the construction of Seed object through Seed.random().

  r(Some(vectorofseq((0 + (9223372036854775807 & 0x7fffffffffffffffL) % 2).toInt)), seed.next).retrieve
  r(Some(vectorofseq((0 + (9223372036854775807) % 2).toInt)), seed.next).retrieve
  r(Some(vectorofseq((0 + 1).toInt)), seed.next).retrieve
  r(Some(vectorofseq(1)), seed.next).retrieve
  vectorofseq(1)
  nonEmptyHeap
  */


