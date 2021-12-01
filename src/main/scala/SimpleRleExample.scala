object SimpleRleExample extends App {

  /*
   *  Seq(1, 2, 2, 3, 4, 3, 3, 3)
   *    =>
   *  Seq((1, 1), (2, 2), (3, 1), (4, 1), (3, 3))
   */

  def compress(ints: Seq[Int]): Seq[(Int, Int)] = {
    ints.foldLeft(Seq.empty[(Int, Int)])((counters, elem)=> {
      (counters, elem) match {
        case (counters, elem) if counters.isEmpty => Seq((elem, 1))
        case (counter :: counters, elem) =>

          val newCounters = counter match {
            case (prev, count) if prev == elem => Seq((prev, count + 1))
            case _ => Seq((elem, 1), counter)
          }

          newCounters ++ counters
      }
    }).reverse
  }

  println(compress(Seq(1, 2, 2, 3, 4, 3, 3, 3)))

}
