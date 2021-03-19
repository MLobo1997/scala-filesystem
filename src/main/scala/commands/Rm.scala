package com.mlobo
package commands

import filesystem.State
import utils.Path

class Rm(path: Path) extends Command {
  def apply(state: State): State = {
    state.removeEntryFromDirectoryTree(
      state.wd.fullpath.join(path).getAbsolutePath
    )
  }
}
