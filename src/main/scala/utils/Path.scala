package com.mlobo
package utils

import utils.Path.{CURRENT_PATH, PARENT_PATH, ROOT_PATH, SEPARATOR}

import scala.annotation.tailrec
import scala.util.matching.Regex

class Path(val listedPath: List[String], val base: String) {
  def join(path: String): Path = {
    val newListedPath: List[String] =
      if (path.nonEmpty) listedPath :+ path else listedPath
    new Path(newListedPath, base)
  }

  def getParent: Path = Path(listedPath.init, base)

  def getLast: String = listedPath.last

  def getAbsolutePath: Path = {
    assert(base == ROOT_PATH)
    @tailrec
    def aux(newPath: List[String], next: List[String]): List[String] =
      next match {
        case Nil => newPath
        case ::(head, tail) =>
          head match {
            case PARENT_PATH  => aux(newPath.init, tail)
            case CURRENT_PATH => aux(newPath, tail)
            case _            => aux(newPath :+ head, tail)
          }
      }
    Path(aux(List(), listedPath), base)
  }

  override def toString: String =
    listedPath.foldLeft(base)((a, b) =>
      if (a != SEPARATOR) a + SEPARATOR + b else a + b
    )
}

object Path {
  val ROOT_PATH = "/"
  val CURRENT_PATH = "."
  val PARENT_PATH = ".."
  val SEPARATOR = "/"
  val ROOT_PATTERN: Regex = s"$ROOT_PATH.*".r

  def apply(listedPath: List[String], basePath: String): Path =
    new Path(listedPath, basePath)

  def apply(string: String): Path = {
    val basePath: String =
      if (ROOT_PATTERN.matches(string)) ROOT_PATH else CURRENT_PATH
    Path(string.split(SEPARATOR).filter(_.nonEmpty).toList, basePath)
  }

}
