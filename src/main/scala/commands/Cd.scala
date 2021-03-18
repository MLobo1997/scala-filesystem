package com.mlobo
package commands
import filesystem.State
import utils.Path

import scala.util.{Failure, Success}

class Cd(val path: Path) extends Command {
  def apply(state: State): State = {
    val destPath = state.wd.fullpath.join(path).getAbsolutePath
    state.root.getDirectoryWithRelativePath(destPath) match {
      case Failure(exception) => state.setMessage(exception.getMessage)
      case Success(directory) => State(state.root, directory)
    }
  }
}
