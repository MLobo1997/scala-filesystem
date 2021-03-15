package com.mlobo
package commands
import filesystem.State

class Pwd extends Command {
  def apply(state: State): State =
    state.setMessage(state.wd.fullpath)
}
