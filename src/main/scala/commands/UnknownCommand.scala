package com.mlobo
package commands

import filesystem.State

class UnknownCommand extends Command {
  def apply(state: State): State = state.setMessage("Command not found!")

}
