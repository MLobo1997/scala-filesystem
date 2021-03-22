package com.mlobo
package commands
import files.File
import filesystem.State
import utils.Path

import scala.util.{Failure, Success}

class Cat(path: Path) extends Command {
  def apply(state: State): State = {
    val filePath = state.root.getEntryWithPath(
      state.wd.fullpath.join(path.getParent).getAbsolutePath,
      path.getLast
    )
    filePath match {
      case Failure(exception)  => state.setMessage(exception.getMessage)
      case Success(file: File) => state.setMessage(file.content)
      case Success(_)          => state.setMessage(s"$path is not a file")
    }
  }
}
