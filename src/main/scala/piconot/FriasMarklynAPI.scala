package piconot


import java.io.File

import picolib.maze.Maze
import picolib.semantics._
import scalafx.application.JFXApp

/**
 * @author Alejandro Frias and Tyler Marklyn
 * @version 4.2.9.26
 * 
 * This DSL for picobot is how it Ought to be written (see what we did there, eh eh?).
 * How to use:
 *  1. create an object that extends PicoOught and passes in the file name for the map
 *          object YourBot extends PicoOught("filename")
 *  2. Program easily using named sections, powerful Go (i.e. move) commands, directional 
 *     based commands, and even simple one line If statements
 *
 * Notes:
 *  1. Remember to predeclare all the section names at the top like so:
 *          Sections("Section 1", "Other section", "You get it", etc.)
 *  2. If statements simply skip the next line if the surroundings don't match. 
 *     You can make it skip chunk by moving that chunk into a separate section
 *  3. It might not work if you have more than 99 sections and more than 99 lines within a section.
 */
class PicoOught(val mapFile: String) {
    // The global state:
    private var section_number = 0                // The section we're currently making rules for
    private var line_number = 0                   // The line within the section
    private var rules: List[Rule] = List()        // The rules generated so far
    private var sections: List[String] = List()   // The list of section names

    // predeclares the sections to be used
    def Sections(args: String*): Unit = {
        sections = args.toList
    }

    // Start Section "section name"
    object Start {
        def Section(label: String): Unit = {
            section_number = sections.indexOf(label) + 1
            line_number = 0
        }
    }

    // Do Section "section name"
    object Do {
        def Section(label: String): Unit = {
            line_number = line_number + 1
            val new_section = sections.indexOf(label) + 1
            if (new_section == -1) println("It looks like you forgot to predeclare a section. :(")
            val newRules = List.range(1,5).map( dir =>
                makeRule(section_number, line_number, dir,
                anySurroundings, StayHere,
                new_section, 1, dir) )
            rules = rules ++ newRules
        }
    }

    // Face (up | right | down | left)
    object Face {
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

    // Turn (left | right | around) 
    object Turn {
        def left = genTurn(3)
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

    // Go direction (once | whilst(conditions))
    // conditions: (wall | open) (in_front | behind | on_left | on_right)
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
                                         section_number, line_number + 1, dir))

                
                case any => rules = rules ++ List.range(1,5).flatMap( dir =>
                    rulesFromDirAndSurr(dir, toDir(dir + dirDiff), conds(dir-1)))
            }
        }
                               

        private def rulesFromDirAndSurr(dirFacing: Int, moveDir: Int, surr: Surroundings): List[Rule] = {
            var someRules: List[Rule] = List(makeRule(section_number, line_number, dirFacing,
                                             surr, dirToMoveDirection(moveDir),
                                             section_number, line_number, dirFacing))
            if(surr.north != Anything) {
                someRules :+= makeRule(section_number, line_number, dirFacing,
                                       Surroundings(opposite(surr.north), Anything, Anything, Anything), StayHere,
                                       section_number, line_number + 1, dirFacing)
            }
            if(surr.east != Anything) {
                someRules :+= makeRule(section_number, line_number, dirFacing,
                                       Surroundings(Anything, opposite(surr.east), Anything, Anything), StayHere,
                                       section_number, line_number + 1, dirFacing)
            }
            if(surr.south != Anything) {
                someRules :+= makeRule(section_number, line_number, dirFacing,
                                       Surroundings(Anything, Anything, Anything, opposite(surr.south)), StayHere,
                                       section_number, line_number + 1, dirFacing)
            }
            if(surr.west != Anything) {
                someRules :+= makeRule(section_number, line_number, dirFacing,
                                       Surroundings(Anything, Anything, opposite(surr.west), Anything), StayHere,
                                       section_number, line_number + 1, dirFacing)
            }
            someRules
        }

    }

    // parts of Go statement
    val once: List[Surroundings] = List()
    def whilst(conds: Map[Int, RelativeDescription]*): List[Surroundings] = {
        val m = conds.reduce(_ ++ _).withDefaultValue(Anything)
        List.range(0,4).map( dir =>  // Don't pay too much attention to these numbers -- So much magic
            Surroundings(m((4 - dir) %4), m((5 - dir) %4), m((7 - dir) %4), m((6 - dir) %4)) ) 
    }

    // Part of making conditions for whilst and If
    object open {
        def in_front = Map(0 -> Open)
        def on_right = Map(1 -> Open)
        def behind = Map(2 -> Open)
        def on_left = Map(3 -> Open)
    }
    object wall {
        def in_front = Map(0 -> Blocked)
        def on_right = Map(1 -> Blocked)
        def behind = Map(2 -> Blocked)
        def on_left = Map(3 -> Blocked)
    }

    // If(conditions) do next line, otherwise skip next line
    def If(conds: Map[Int, RelativeDescription]*): Unit = {
        val m = conds.reduce(_ ++ _).withDefaultValue(Anything)
        val surrs = List.range(0,4).map( dir =>  // Don't pay too much attention to these numbers -- So much magic
            Surroundings(m((4 - dir) %4), m((5 - dir) %4), m((7 - dir) %4), m((6 - dir) %4)) )
        line_number = line_number + 1
        val newRules = List.range(1,5).flatMap(dirFacing => List(
            makeRule(section_number, line_number, dirFacing,
                     surrs(dirFacing - 1), StayHere,
                     section_number, line_number + 1, dirFacing),
            makeRule(section_number, line_number, dirFacing,
                     reverseSurr(surrs(dirFacing - 1)), StayHere,
                     section_number, line_number + 2, dirFacing))
        )
        rules = rules ++ newRules
    }

    // Helper functions for internal use only
    private def opposite(relDesc: RelativeDescription): RelativeDescription = relDesc match {
        case Blocked => Open
        case Open => Blocked
        case Anything => Anything
        
    }

    private def reverseSurr(surr: Surroundings): Surroundings = {
        Surroundings(opposite(surr.north), opposite(surr.east), opposite(surr.west), opposite(surr.south))
    }

    private def toDir(in: Int) = ((in - 1) % 4) + 1

    private val dirToMoveDirection = Map( 1 -> North, 2 -> East, 3 -> South, 4 -> West )
    
    private val anySurroundings = Surroundings(Anything, Anything, Anything, Anything)

    private def makeRule(start_section: Int, start_line: Int, start_dir: Int,
                 surroundings: Surroundings, move_dir: MoveDirection,
                 end_section: Int, end_line: Int, end_dir: Int) = {
        Rule(State(start_section + "0" + start_line + "0" + start_dir),
                    surroundings,
                    move_dir,
                    State(end_section + "0" + end_line + "0" + end_dir)
                    )
    }

    // Launch the app
    def main(args: Array[String]): Unit = {
      println("PicoOught generated " + rules.length + " rules for your enjoyment.")
      val app = new RunApp(rules, mapFile)
      app.main(Array())
    }
    
}

// Helper class for main to launch the JFXApp app
class RunApp(val some_rules: List[Rule], val map: String) extends JFXApp {
    val emptyMaze = Maze("resources" + File.separator + map)

     object EmptyBot extends Picobot(emptyMaze, some_rules)
         with TextDisplay with GUIDisplay

     stage = EmptyBot.mainStage
}
