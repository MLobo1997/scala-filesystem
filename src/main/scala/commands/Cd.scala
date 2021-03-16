package com.mlobo
package commands
import filesystem.State

import scala.util.{Failure, Success}

class Cd(val path: String) extends Command {
  def apply(state: State): State = {
    val splittedPath: List[String] = path.split("/").toList
    state.wd.getDirectoryWithRelativePath(
      splittedPath.init.foldLeft("")((a, b) =>
        if (a.nonEmpty) a + '/' + b else b
      ),
      splittedPath.last
    ) match {
      case Failure(exception) => state.setMessage(exception.getMessage)
      case Success(directory) => State(state.root, directory, "")
    }
  }
}
