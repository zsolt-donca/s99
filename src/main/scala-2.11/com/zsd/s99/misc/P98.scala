package com.zsd.s99.misc

object P98 extends App {

  sealed trait Position
  case object Unknown extends Position
  case object Filled extends Position
  case object Empty extends Position

  case class Line(contents: Seq[Position]) {
    def length = contents.length
    def update(pos: Int, position: Position) = Line(contents.updated(pos, position))

    def ++(line: Line): Line = Line(contents ++ line.contents)
    def ++(position: Position): Line = this ++ Line(Seq(position))
  }

  implicit class TimesLineFactory(length: Int) {
    import Seq.fill
    def unknown = Line(fill(length)(Unknown))
    def filled = Line(fill(length)(Filled))
    def empty = Line(fill(length)(Empty))
  }

  implicit class StringLineFactory(str: String) {
    def asLine = Line(str.collect({
      case '?' => Unknown
      case 'o' => Filled
      case 'x' => Empty
      case _ => throw new IllegalArgumentException
    }))
  }

  case class Group(length: Int)
  case class Groups(contents: Group*)
  case class GroupWithConstraint(group: Group, constraint: Range)

  implicit def numberAsGroup(length: Int): Group = new Group(length)

  def constraints(groups: Groups, line: Line): Seq[Range] = {
    if (line.contents.exists(_ != Unknown)) {
      ???
    } else {
      if (groups.contents.length == 1) {
        Seq(0 until line.length)
      } else {
        val dividedGroups = divideGroups(groups)
        dividedGroups collect {
          case (before, elem, after) => minSize(before) until (line.length - minSize(after))
        }
      }
    }
  }

  def minSize(groups: Seq[Group]): Int = {
    if (groups.isEmpty) 0 else groups.map(_.length).sum + groups.length
  }

  def divideGroups(groups: Groups): Seq[(Seq[Group], Group, Seq[Group])] = {
    groups.contents.indices.map(i => {
      val before = groups.contents.slice(0, i)
      val elem = groups.contents(i)
      val after = groups.contents.slice(i + 1, groups.contents.length)
      (before, elem, after)
    })
  }

  def fillKnownPositions(groupLengths: Seq[Int], constraints: Seq[Range], line: Line): Line = {
    groupLengths.zip(constraints).foldLeft(line)((line, groupLengthsAndConstraints) => {
      groupLengthsAndConstraints match {
        case (groupLength, constraint) =>
          val diff = constraint.length - groupLength
          if (diff < groupLength) {
            ((constraint.start + diff) to (constraint.end - diff)).foldLeft(line)((line, i) => line(i) = Filled)
          } else {
            line
          }
      }
    })
  }


}
