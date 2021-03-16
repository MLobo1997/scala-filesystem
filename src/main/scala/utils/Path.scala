package com.mlobo
package utils

import utils.Path.{CURRENT_PATH, ROOT_PATH, SEPARATOR}

class Path(string: String) {
  val listedPath: List[String] =
    string.split(SEPARATOR).filter(_.nonEmpty).toList
  val basePath: Char = string.headOption match {
    case Some(char) =>
      char match {
        case ROOT_PATH => ROOT_PATH
        case _         => CURRENT_PATH
      }
    case None => CURRENT_PATH
  }
}

object Path {
  val CURRENT_PATH = '.'
  val ROOT_PATH = '/'
  val SEPARATOR = '/'

  def apply(string: String): Path =
    new Path(string)
}
