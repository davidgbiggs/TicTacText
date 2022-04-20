import scala.collection.mutable

object Game {

  def main(args: Array[String]): Unit = {
    mainMenu()
  }

  def mainMenu(): Unit = {

    println("\nWelcome to Tic Tac Text. What would you like to do? Type the number that is beside your selection.\n" +
      "\n1. 1 Player\n2. 2 Player\n3. How to Play\n")

    val response: String = scala.io.StdIn.readLine()
    if (response == "1") {
      gameFlow(false)
    } else if (response == "2") {
      gameFlow(true)
    } else if (response == "3") {
      gameExplanation()
    } else {
      println("That is not a valid option.")
      mainMenu()
    }
  }

  def gameFlow(is2Player: Boolean): Unit = {
    // initialize game state
    val gameState: mutable.LinkedHashMap[String, Char] = scala.collection.mutable.LinkedHashMap[String, Char]("11" -> '■',
      "12" -> '■', "13" -> '■', "21" -> '■', "22" -> '■', "23" -> '■', "31" -> '■', "32" -> '■', "33" -> '■')

    // initialize player data structures
    val p1 = new Player("Player 1", 'X', false)
    val p2 = new Player("Player 2", 'O', false)

    if (is2Player) {
      printGameState(gameState)
      // print the winner if there is one; if not, proclaim a tie and ask if another game is desired
      try {
        println("\n" + nextTurn(p1, p2, gameState).get.name + " wins!")
      }
      catch {
        case _: Throwable => println("\nIt's a tie!")
      }
      finally {
        mainMenu()
      }
    }
    else {
      println("AI has not been implemented yet! Come back another time!")
      mainMenu()
    }
  }

  def printGameState(gameState: mutable.LinkedHashMap[String, Char]): Unit = {
    for ((k, v) <- gameState) if (k == "13" || k == "23" || k == "33") println(v + " ") else print(v + " ")
  }

  /** Switches between players until the end of the game */
  def nextTurn(turnPlayer: Player, otherPlayer: Player, state: mutable.LinkedHashMap[String, Char]): Option[Player] = {

    println("\nWhere would you like to play, " + turnPlayer.name + " (" + turnPlayer.symbol + ")" + "? (type row # then column # eg. '13' for top-right)\n")
    var move = scala.io.StdIn.readLine()

    // check if it is a valid move and loop until a legitimate move is performed
    while ((move != "11" && move != "12" && move != "13" && move != "21" && move != "22" && move != "23" && move != "31"
      && move != "32" && move != "33") || state(move) == otherPlayer.symbol || state(move) == turnPlayer.symbol) {

      println("\nThat space is already taken or does not exist. Try again.\n")
      move = scala.io.StdIn.readLine()

    }

    state(move) = turnPlayer.symbol

    printGameState(state)

    // check if the latest move won, tied, or another turn is necessary
    if (didWin(state)) Option(turnPlayer)
    else if (state.forall { case (_, v) => v != '■' }) None
    else nextTurn(otherPlayer, turnPlayer, state)
  }

  /** Checks for win conditions given a game state. */
  def didWin(state: mutable.LinkedHashMap[String, Char]): Boolean = {
    // this function returns true if a win condition is met
    if (state("11") == state("12") && state("11") == state("13") && state("11") != '■') true
    else if (state("21") == state("22") && state("21") == state("23") && state("21") != '■') true
    else if (state("31") == state("32") && state("31") == state("33") && state("31") != '■') true
    else if (state("11") == state("21") && state("11") == state("31") && state("11") != '■') true
    else if (state("12") == state("22") && state("12") == state("32") && state("12") != '■') true
    else if (state("13") == state("23") && state("13") == state("33") && state("13") != '■') true
    else if (state("11") == state("22") && state("11") == state("33") && state("11") != '■') true
    else if (state("13") == state("22") && state("13") == state("31") && state("13") != '■') true
    else false
  }

  def gameExplanation(): Unit = {

    println("\nThis is a game of Tic Tac Toe, played via the command line.\nYou can either play against an AI opponent," +
      " or someone on your local machine by switching turns.\nTo make a move, you type the row number, counted from top" +
      " to bottom, followed immediately by the column number, counted from left to right.\nFor example, to make a move" +
      " in the top-right, you would type '13', and to make a move in the middle, you would type '22', and so on.")

    mainMenu()
  }
}