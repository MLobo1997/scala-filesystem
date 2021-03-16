package com.mlobo
package commands
import filesystem.State

import com.mlobo.utils.Path

import scala.util.{Failure, Success}

class Cd(val path: Path) extends Command {
  def apply(state: State): State = {
    state.wd.getDirectoryWithRelativePath(
      path.getParent,
      path.getLast
    ) match {
      case Failure(exception) => state.setMessage(exception.getMessage)
      case Success(directory) => State(state.root, directory, "")
    }
  }
}
