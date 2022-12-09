import quickcheck.QuickCheckHeap

val q = new QuickCheckHeap with quickcheck.test.Bogus4BinomialHeap
q.isEmpty(Nil)
q.insert(0, Nil)