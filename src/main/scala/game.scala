import scala.collection.mutable

class Player(name: String, identity: Char) {
  val title: String = name
  val symbol: Char = identity
}

object game {
  var state: mutable.LinkedHashMap[String, Char] = scala.collection.mutable.LinkedHashMap[String, Char]("11" -> 'e',
    "12" -> 'e', "13" -> 'e', "21" -> 'e', "22" -> 'e', "23" -> 'e', "31" -> 'e', "32" -> 'e', "33" -> 'e')

  val p1 = new Player("Player 1", 'x')
  val p2 = new Player("Player 2", 'o')

  def didWin(): Boolean = {
    // this function returns true if a win condition is met
    if (state("11") == state("12") && state("11") == state("13") && state("11") != 'e') true
    else if (state("21") == state("22") && state("21") == state("23") && state("21") != 'e') true
    else if (state("31") == state("32") && state("31") == state("33") && state("31") != 'e') true
    else if (state("11") == state("21") && state("11") == state("31") && state("11") != 'e') true
    else if (state("12") == state("22") && state("12") == state("32") && state("12") != 'e') true
    else if (state("13") == state("23") && state("13") == state("33") && state("13") != 'e') true
    else if (state("11") == state("22") && state("11") == state("33") && state("11") != 'e') true
    else if (state("13") == state("22") && state("13") == state("31") && state("13") != 'e') true
    else false
  }

  def turn(player1: Player, player2: Player): Unit = {

    println("")
    println("Where would you like to play, " + player1.title + " (" + player1.symbol + ")" + "?")

    var move = scala.io.StdIn.readLine()

    while ((move != "11" && move != "12" && move != "13" && move != "21" && move != "22" && move != "23" && move != "31"
      && move != "32" && move != "33") || state(move) == player2.symbol || state(move) == player1.symbol) {

      println("That space is already taken or does not exist. Try again.")
      move = scala.io.StdIn.readLine()

    }

    state(move) = player1.symbol

    for ((k, v) <- state) if (k == "13" || k == "23" || k == "33") println(v + " ") else print(v + " ")
    if (didWin()) println(player1.title + " wins!")
    else if (state.forall{ case (_, v) => v != 'e' }) println("It's a tie!")
    else turn(player2, player1)
  }

  def main(args: Array[String]): Unit = {
    turn(p1, p2)
  }
}