package com.zsd.s99.mtrees

object P73 {

  implicit class LispyTreeConverter(val mTree: MTree[_]) {

    def lispyTree: String = {
      mTree match {
        case MTree(value, List()) => value.toString
        case MTree(value, children) => s"($value ${children.map(_.lispyTree).mkString(" ")})"
      }
    }
  }

}