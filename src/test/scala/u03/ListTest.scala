package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.AlgebraicDataTypes.Person
import u02.AlgebraicDataTypes.Person.Teacher
import u02.Optionals.Option.*

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map2(l)(_ + 1))
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map2(l)(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testFilterMap(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filterMap(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterMap(l)(_ != 20))

  @Test def testDrop(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))

  @Test def testAppend(): Unit =
    val tail = Cons(40, Nil())
    assertEquals( Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMax(): Unit =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

  @Test def testGetCourses(): Unit =
    val teachers: List[Person] = Cons(Teacher("mario", "algebra"), Cons(Teacher("paolo", "analysis"), Nil()))
    assertEquals(Cons("algebra", Cons("analysis",Nil())), getCourses(teachers))
