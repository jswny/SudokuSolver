import Solution._

class Test extends org.scalatest.FunSuite {
  test("solve 1") {
    val test = "85....4.1......67...21....3..85....7...982...3....15..5....43...37......2.9....58"
    val parsed = Solution.parse(test)
    val solved = parsed.solve()
    println("Solved board:")
    println(solved.get)
  }

  test("solve 2") {
    val test = ".1.....2..3..9..1656..7...33.7..8..........89....6......6.254..9.5..1..7..3.....2"
    val parsed = Solution.parse(test)
    val solved = parsed.solve()
    println("Solved board:")
    println(solved.get)
  }

  test("solve 3") {
    val test = ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71."
    val parsed = Solution.parse(test)
    val solved = parsed.solve()
    println("Solved board:")
    println(solved.get)
  }

  test("solve 4") {
    val test = "2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3"
    val parsed = Solution.parse(test)
    val solved = parsed.solve()
    println("Solved board:")
    println(solved.get)
  }
}
