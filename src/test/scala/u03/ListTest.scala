package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*

class ListTest:
  import List.*
  /*
    - Type List[A]
    - Constructors:
        Cons(head: A, tail: List[A])
        Nil()
    - Operations:
        sum: List[Int] -> Int
        get[E]: List[E] x Int -> Option[E]
        add[E]: List[E] x Int x E -> Option[List[E]]
        drop[E]: List[E] x Int -> Option[List[E]]
        append[E]: List[E] x List[E] -> List[E]

    - Axioms:
        sum(Nil()) = 0
        sum(Cons(h,t)) = h + sum(t)
        get(Nil,i) = None()
        get(Cons(h,t),0) = Some(h) // lista non vuota, almeno h presente
        get(Cons(h,t),i) = et(t, i-1)
        drop(Nil,i) = None()
        drop(Cons(h,t), i) = List
        append(Cons(h,t), l) = Cons(h, append(t,l))
        append(l, Nil) = l
  */

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_+""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_!=20))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Nil(), drop(l, 5))
    assertEquals(Cons(30, Nil()), drop(l, 2))

  @Test def testAppend() =
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), append(Nil(), l))
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v+1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v+1, Cons(v+2, Nil()))))

  @Test def testMapWithFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapWithFlatMap(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapWithFlatMap(l)(_ + ""))


