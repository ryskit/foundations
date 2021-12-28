package exercises.dataprocessing

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

// For example, here is a ParList[Int] with two partitions:
// ParList(
//  List(1,2,3,4,5,6,7,8), // partition 1
//  List(9,10)             // partition 2
// )
// Note that we used the `apply` method with a varargs argument.
case class ParList[A](partitions: List[List[A]]) {
  def toList: List[A] = partitions.flatten

  def map[To](update: A => To): ParList[To] =
    ParList(partitions.map(_.map(update)))

  /** Please use default value such as when used with combine it is a no-operation * */
  def monoFoldLeft(param: Monoid[A]): A =
    partitions.map(_.foldLeft(param.default)(param.combine)).foldLeft(param.default)(param.combine)

  def size: Int =
    foldMap(_ => 1)(Monoid.sumInt)

  def foldMap[To](update: A => To)(monoid: Monoid[To]): To =
    partitions
      .map { partition =>
        partition.foldLeft(monoid.default)((state: To, value: A) => monoid.combine(state, update(value)))
      }
      .foldLeft(monoid.default)(monoid.combine)
}

object ParList {
  // The `*` at the end of List[A] is called a varargs. It means we can put as many arguments
  // as we want of type List[A] and the Scala compiler will automatically packs these arguments
  // into a collection.
  // For example, ParList(List(1,2), List(3,4)) == ParList(List(List(1,2), List(3,4)))
  // This is why we can create a List using the syntax List(1,2,3) instead of 1 :: 2 :: 3 :: Nil
  def apply[A](partitions: List[A]*): ParList[A] =
    ParList(partitions.toList)

  // Creates a ParList by grouping a List into partitions of fixed size.
  // If the length of input list is not divisible by the partition size, then
  // the last partition will be smaller. For example:
  // byPartitionSize(3, List(1,2,3,4,5,6,7,8,9,10)) == ParList(
  //   List(1,2,3),
  //   List(4,5,6),
  //   List(7,8,9),
  //   List(10)
  // )
  def byPartitionSize[A](partitionSize: Int, items: List[A]): ParList[A] =
    if (items.isEmpty) ParList()
    else ParList(items.grouped(partitionSize).toList)
}
