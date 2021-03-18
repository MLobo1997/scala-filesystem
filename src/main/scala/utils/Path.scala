package com.mlobo
package utils

import utils.Path.{
  CURRENT_PATH,
  PARENT_PATH,
  ROOT_PATH,
  ROOT_PATTERN,
  SEPARATOR
}

import scala.annotation.tailrec
import scala.util.matching.Regex

class Path(val listedPath: List[String], val isRoot: Boolean) {
  def join(path: String): Path = join(Path(path))

  def join(path: Path): Path =
    if (path.isRoot) path
    else Path(listedPath ++ path.listedPath, isRoot)

  def getParent: Path = Path(listedPath.init, isRoot)

  def getLast: String = listedPath.last

  def getAbsolutePath: Path = {
    assert(isRoot)
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
    Path(aux(List(), listedPath), isRoot)
  }

  override def toString: String =
    listedPath.foldLeft(if (isRoot) ROOT_PATH else CURRENT_PATH)((a, b) =>
      a match {
        case CURRENT_PATH => b
        case ROOT_PATH    => a + b
        case _            => a + SEPARATOR + b
      }
    )
}

object Path {
  val ROOT_PATH = "/"
  val CURRENT_PATH = "."
  val PARENT_PATH = ".."
  val SEPARATOR = "/"
  val ROOT_PATTERN: Regex = s"$ROOT_PATH.*".r

  def apply(listedPath: List[String], isRoot: Boolean = false): Path =
    new Path(listedPath, isRoot)

  def apply(string: String): Path = {
    val isRoot: Boolean = ROOT_PATTERN matches string
    Path(fromStringToList(string), isRoot)
  }

  def fromStringToList(string: String): List[String] =
    string.split(SEPARATOR).filter(_.nonEmpty).toList
}
