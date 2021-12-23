package exercises.dataprocessing

import exercises.dataprocessing.ForLoopExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ForLoopExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
  }

  test("sum is consistent with List sum") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("size") {
    assert(size(List(2, 5, 1, 8)) == 4)
    assert(size(Nil) == 0)
  }

  test("size and concat") {
    forAll { (list1: List[Int], list2: List[Int]) =>
      val size1 = list1.size
      val size2 = list2.size
      assert(size(list1 ++ list2) == size1 + size2)
    }
  }

  test("min") {
    assert(min(List(2, 5, 1, 8)) == Some(1))
    assert(min(Nil) == None)
  }

  test("min returns a value smaller than all elements in input list") {
    forAll { (numbers: List[Int]) =>
      for {
        minValue <- min(numbers)
        number   <- numbers
      } assert(minValue <= number)
    }
  }

  test("min returns a value that belongs to the input list") {
    forAll { (numbers: List[Int]) =>
      min(numbers).foreach(minValue => numbers.contains(minValue))
    }
  }

  test("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

  test("wordCount returns frequencies > 0") {
    forAll { (words: List[String]) =>
      wordCount(words).values.foreach(frequencies => assert(frequencies > 0))
    }
  }

  test("foldLeft process inputs in orders") {
    forAll { (numbers: List[Int]) =>
      assert(foldLeft(numbers, List.empty[Int])((prefix, number) => prefix :+ number) == numbers)
    }
  }
}
