package piconot


import java.io.File

import picolib.maze.Maze
import picolib.semantics._
// import picolib.semantics.Anything
// import picolib.semantics.Blocked
// import picolib.semantics.East
// import picolib.semantics.GUIDisplay
// import picolib.semantics.North
// import picolib.semantics.Open
// import picolib.semantics.Picobot
// import picolib.semantics.Rule
// import picolib.semantics.South
// import picolib.semantics.State
// import picolib.semantics.StayHere
// import picolib.semantics.Surroundings
// import picolib.semantics.TextDisplay
// import picolib.semantics.West
import scalafx.application.JFXApp


class FriasMarklynAPI {
    var section_number = 0
    var line_number = 0
    var rules: List[Rule] = List() //TODO so ugly

    var sections: List[String] = List()

    val dirToMoveDirection = Map( 1 -> North, 2 -> East, 3 -> South, 4 -> West )
    
    def Sections(args: String*): Unit = {
        sections = args.toList
    }

    val anySurroundings = Surroundings(Anything, Anything, Anything, Anything)


    trait FaceTrait {
        def up = genFace(1)
        def right = genFace(2)
        def down = genFace(3)
        def left = genFace(4)

        def genFace(toFace: Int): Unit = {
        line_number = line_number + 1
        val newRules = List.range(1,5).map(dir => makeRule(
            section_number, line_number, dir,
            anySurroundings, StayHere,
            section_number, line_number + 1, toFace
            ))
        rules = rules ++ newRules
        }
    }

    object Face extends FaceTrait

    trait TurnTrait {
        def left = genTurn(-1)
        def right = genTurn(1)
        def around = genTurn(2)

        def genTurn(turnAmount: Int): Unit = {
            line_number = line_number + 1;
            val newRules = List.range(1,5).map(dir => makeRule(
                section_number, line_number, dir,
                anySurroundings, StayHere,
                section_number, line_number + 1, toDir(dir + turnAmount)
                ))
            rules = rules ++ newRules
        }
    }

    object Turn extends TurnTrait

    def toDir(in: Int) = ((in - 1) % 4) + 1


    object New {
        def Section(label: String): Unit = {
            section_number = sections.indexOf(label) + 1
            line_number = 0
        }
    }

    object Do {
        def Section(label: String): Unit = {
            line_number = line_number + 1
            val new_section = sections.indexOf(label) + 1
            val newRules = List.range(1,5).map( dir =>
                makeRule(section_number, line_number, dir,
                anySurroundings, StayHere,
                new_section, 1, dir) )
            rules = rules ++ newRules
        }
    }

    val once: List[Surroundings] = List()

    object Go {
        def forwards(conds: List[Surroundings]) = makeGo(0, conds)

        def right(conds: List[Surroundings]) = makeGo(1, conds)

        def backwards(conds: List[Surroundings]) = makeGo(2, conds)

        def left(conds: List[Surroundings]) = makeGo(3, conds)

        private def makeGo(dirDiff: Int, conds: List[Surroundings]) = {
            line_number = line_number + 1
            conds match {
                case Nil => rules = rules ++ List.range(1,5).map( dir => 
                                makeRule(section_number, line_number, dir,
                                anySurroundings, dirToMoveDirection(toDir(dir + dirDiff)),
                                section_number, line_number + 1, dir ))
                
                case any => println("while not yet implemented")
            }
        }
    }

    // class Condition {
    //     def wall_on = new 

    //     def open_on = 
    // }



    def makeRule(start_section: Int, start_line: Int, start_dir: Int,
                 surroundings: Surroundings, move_dir: MoveDirection,
                 end_section: Int, end_line: Int, end_dir: Int) = {
        Rule(State(start_section + "0" + start_line + "0" + start_dir),
                    surroundings,
                    move_dir,
                    State(end_section + "0" + end_line + "0" + end_dir)
                    )
    }
}



// Stretch Goals:
//    # comments instead of //
//    If statements
//    spaces
//    pre-parsing to get rid of "" and {}
