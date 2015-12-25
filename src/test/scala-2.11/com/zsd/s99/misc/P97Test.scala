package com.zsd.s99.misc

import org.scalatest.{FlatSpec, Matchers}

class P97Test extends FlatSpec with Matchers {

  import P97._

  val sampleProblem =
    """
      |.  .  4 | 8  .  . | .  1  7
      |6  7  . | 9  .  . | .  .  .
      |5  .  8 | .  3  . | .  .  4
      |--------+---------+--------
      |3  .  . | 7  4  . | 1  .  .
      |.  6  9 | .  .  . | 7  8  .
      |.  .  1 | .  6  9 | .  .  5
      |--------+---------+--------
      |1  .  . | .  8  . | 3  .  .
      |.  .  . | .  .  6 | .  9  1
      |2  4  . | .  .  1 | 5  .  .
    """.stripMargin

  val sampleSolution =
    """
      |9  3  4 | 8  2  5 | 6  1  7
      |6  7  2 | 9  1  4 | 8  5  3
      |5  1  8 | 6  3  7 | 9  2  4
      |--------+---------+--------
      |3  2  5 | 7  4  8 | 1  6  9
      |4  6  9 | 1  5  3 | 7  8  2
      |7  8  1 | 2  6  9 | 4  3  5
      |--------+---------+--------
      |1  9  7 | 5  8  2 | 3  4  6
      |8  5  3 | 4  7  6 | 2  9  1
      |2  4  6 | 3  9  1 | 5  7  8
    """.stripMargin

  "The sample Sudoku problem" should "be resolved as expected" in {

    val problem = Sudoku(sampleProblem)
    val expectedSolution = Sudoku(sampleSolution)

    println("Problem:")
    println(problem)

    println("Solution:")
    println(expectedSolution)

    val actualSolution = Sudoku.solve(problem)

    actualSolution.size should be(1)
    actualSolution.head should be(expectedSolution)
  }

}
