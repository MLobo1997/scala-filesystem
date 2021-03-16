package com.mlobo
package utils

import utils.Path.SEPARATOR

class Path(val listedPath: List[String], val basePath: Char) {
  def join(path: String): Path = {
    val newListedPath: List[String] =
      if (path.nonEmpty) listedPath :+ path else listedPath
    new Path(newListedPath, basePath)
  }

  def getParent: Path = Path(listedPath.init, basePath)

  def getLast: String = listedPath.last

  override def toString: String =
    listedPath.foldLeft(basePath.toString)((a, b) =>
      if (a != SEPARATOR.toString) a + SEPARATOR + b else a + b
    )
}

object Path {
  val CURRENT_PATH = '.'
  val ROOT_PATH = '/'
  val SEPARATOR = '/'

  def apply(listedPath: List[String], basePath: Char): Path =
    new Path(listedPath, basePath)

  def apply(string: String): Path = {
    val basePath: Char = string.headOption match {
      case Some(char) =>
        char match {
          case ROOT_PATH => ROOT_PATH
          case _         => CURRENT_PATH
        }
      case None => CURRENT_PATH
    }
    Path(string.split(SEPARATOR).filter(_.nonEmpty).toList, basePath)
  }

}
