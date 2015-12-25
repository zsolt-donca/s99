package com.zsd.s99.misc

import java.util.regex.Matcher
import java.util.regex.Pattern.compile

import scala.annotation.tailrec
import scala.collection.parallel.ParSeq

object P97 extends App {

  case class Sudoku(state: Vector[Int]) {
    if (state.size != 81) {
      throw new IllegalArgumentException("Size: " + state.size)
    }

    def apply(row: Int, col: Int): Int = state(row * 9 + col)

    def apply(bigRow: Int, smallRow: Int, bigCol: Int, smallCol: Int): Int = this (bigRow * 3 + smallRow, bigCol * 3 + smallCol)

    lazy val solved = !state.contains(0)

    class Chunk(val values: Seq[Int]) {
      lazy val usedValues: Set[Int] = values.filter(_ > 0).toSet

      lazy val freeValues = (1 to 9).toSet.diff(usedValues)

      def ++(chunk: Chunk): Chunk = new Chunk(values ++ chunk.values)

      val sudoku = Sudoku.this
    }

    lazy val rows: Seq[Chunk] = {
      for (row <- 0 to 8)
        yield new Chunk(
          for (col <- 0 to 8)
            yield this (row, col)
        )
    }

    lazy val cols: Seq[Chunk] = {
      for (col <- 0 to 8)
        yield new Chunk(
          for (row <- 0 to 8)
            yield this (row, col)
        )
    }

    lazy val squares: Seq[Chunk] = {
      for (bigRow <- 0 to 2; bigCol <- 0 to 2)
        yield new Chunk(
          for (smallRow <- 0 to 2; smallCol <- 0 to 2)
            yield this (bigRow, smallRow, bigCol, smallCol)
        )
    }

    lazy val conflicts: Boolean = {
      def sectionThatConflicts(section: Chunk) = {
        val values = section.usedValues
        values.toSet.size != values.size
      }
      def conflicts(stuff: Seq[Chunk]): Boolean = stuff.exists(sectionThatConflicts)

      conflicts(rows) || conflicts(cols) || conflicts(squares)
    }

    class Position(val index: Int) {
      val rowIndex = index / 9
      val colIndex = index % 9
      val row: Chunk = rows(rowIndex)
      val col: Chunk = cols(colIndex)
      val square: Chunk = squares((rowIndex / 3) * 3 + colIndex / 3)

      lazy val union = row ++ col ++ square
      lazy val freeValues = if (state(index) != 0) Set() else union.freeValues
      lazy val trivial = freeValues.size == 1
    }

    lazy val positions = state.indices.map(chunksForPos)

    def chunksForPos(i: Int) = new Position(i)

    def updated(index: Int, value: Int) = new Sudoku(state.updated(index, value))

    override def toString: String = {
      (for (bigRow <- 0 to 2)
        yield (for (smallRow <- 0 to 2)
          yield (for (bigCol <- 0 to 2)
            yield (for (smallCol <- 0 to 2)
              yield {
                val value = this (bigRow, smallRow, bigCol, smallCol)
                if (value == 0) "." else value.toString
              }
              ).mkString("  ")
            ).mkString(" | ")
          ).mkString("\n")
        ).mkString("\n--------+---------+--------\n")
    }
  }

  object Sudoku {
    def apply(str: String): Sudoku = {
      val matcher: Matcher = compile("\\d+|\\.").matcher(str)
      var numbers = Vector[Int]()
      while (matcher.find()) {
        val value = if (matcher.group != ".") matcher.group.toInt else 0
        numbers = numbers ++ Vector(value)
      }
      new Sudoku(numbers)
    }

    @tailrec
    private def solveTrivial(problem: Sudoku): Sudoku = {
      val trivialPositions = problem.positions.filter(_.trivial)
      if (trivialPositions.nonEmpty) {
        val solution = trivialPositions.foldLeft(problem)((sudoku, position) => sudoku.updated(position.index, position.freeValues.toSeq.head))
        solveTrivial(solution)
      } else {
        problem
      }
    }

    private def solveCombinations(problem: Sudoku): ParSeq[Sudoku] = {
      val freePositions = problem.positions
        .filter(_.freeValues.nonEmpty)
        .sortBy(_.freeValues.size)

      for (position <- freePositions.par;
           num <- position.union.freeValues.par;
           updatedSudoku = problem.updated(position.index, num);
           solution <- solve(updatedSudoku))
        yield solution
    }

    def solve(problem: Sudoku): ParSeq[Sudoku] = {
      val trivialSolution = solveTrivial(problem)
      if (trivialSolution.solved) {
        ParSeq(trivialSolution)
      } else {
        solveCombinations(trivialSolution)
      }
    }
  }

}
