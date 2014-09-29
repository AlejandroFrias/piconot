package piconot

import scala.language.postfixOps

import java.io.File

import picolib.maze.Maze
import picolib.semantics.GUIDisplay
import picolib.semantics.Picobot
import picolib.semantics.TextDisplay
import scalafx.application.JFXApp



object Empty extends FriasMarklynAPI("empty.txt") {

    Sections("start", "fill");

    New Section "start";
        Face up;
        Go forwards whilst(open in_front);
        Go left whilst(open on_left);
        Do Section "fill";

    New Section "fill";
        Go backwards whilst(open behind);
        Go forwards whilst(open in_front);
        Go right once_if_possible;
        Do Section "fill"
}
