package com.mlobo.scala.filesystem

import com.mlobo.scala.commands.Command
import com.mlobo.scala.files.Directory

import java.util.Scanner

object Filesystem extends App {
  val root = Directory.ROOT
  var state = State(root, root) //vars is only for
  val scanner = new Scanner(System.in)

  while (true) {
    state.show
    val input = scanner.nextLine()
    state = Command.from(input)(state)
  }
}
