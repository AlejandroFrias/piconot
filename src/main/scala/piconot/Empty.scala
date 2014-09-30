package piconot

import scala.language.postfixOps

object Empty extends PicoOught("empty.txt") {

    Sections("start", "fill");

    Start Section "start";
        Face up;
        Go forwards whilst(open in_front);
        Go left whilst(open on_left);
        Do Section "fill";

    Start Section "fill";
        Go backwards whilst(open behind);
        Go forwards whilst(open in_front);
        Go right once;
        Do Section "fill";
}
