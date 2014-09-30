package piconot

import scala.language.postfixOps

object RightHand extends PicoOught("maze.txt") {

    Sections("Right Hand Rule", "Move and turn right");

    Start Section "Right Hand Rule";
        Go forwards whilst(wall on_right, open in_front);
        If(open on_right);
            Do Section "Move and turn right"
        Turn left;
        Do Section "Right Hand Rule";

    Start Section "Move and turn right";
        Turn right;
        Go forwards once;
        Do Section "Right Hand Rule";

}