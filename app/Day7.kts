import java.io.File

typealias Row = List<Char>
typealias Grid = List<Row>

fun main() {
    println("Day 7")

    if (args.size != 1) {
        error("Invalid arguments, expecting a file")
    }


    val grid: Grid = File(args[0]).readLines().map { it.toList() }
    runBeam(grid)
    runManyWorlds(grid)
}

fun getBeamStartingPosition(row: Row): Int = row.indexOf('S')

fun runBeam(grid: Grid) {
    val startingPosition = getBeamStartingPosition(grid[0])
    if (startingPosition == -1) {
        throw ArrayIndexOutOfBoundsException("Could not find starting position")
    }

    val (finalSplits, _) = grid.drop(1)
        .fold(Pair(0, listOf(startingPosition))) { (currentSplits, currentIndices), nextRow ->
            val (newSplits, nextIndices) = stepWithBeamSplits(nextRow, currentIndices)
            Pair(currentSplits + newSplits, nextIndices)
        }

    println("Final splits: ${finalSplits}")
}

fun stepWithBeamSplits(nextRow: Row, beamIndices: List<Int>): Pair<Int, List<Int>> {
    val newBeamIndices = mutableSetOf<Int>()
    var splits = 0

    beamIndices.forEach { position ->
        when (nextRow[position]) {
            '^' -> {
                splits += 1
                newBeamIndices.add(position - 1)
                newBeamIndices.add(position + 1)
            }

            else -> newBeamIndices.add(position)
        }
    }

//    val visualizedRow = nextRow.toMutableList()
//    newBeamIndices.forEach { i -> visualizedRow[i] = '|' }
//    println(visualizedRow.joinToString(separator = ""))

    return Pair(splits, newBeamIndices.toList())
}

fun runManyWorlds(grid: Grid) {
    val startingPosition = getBeamStartingPosition(grid[0])
    if (startingPosition == -1) {
        throw ArrayIndexOutOfBoundsException("Could not find starting position")
    }

    val totalPaths = grid.drop(1)
        .fold(mapOf(startingPosition to 1L)) { currentPositions, nextRow ->
            val nextPositionsWithCounts = mutableMapOf<Int, Long>()

            currentPositions.forEach { (pos, count) ->
                when (nextRow[pos]) {
                    '^' -> {
                        val leftCount = nextPositionsWithCounts.getOrDefault(pos - 1, 0L) + count
                        nextPositionsWithCounts[pos - 1] = leftCount

                        val rightCount = nextPositionsWithCounts.getOrDefault(pos + 1, 0L) + count
                        nextPositionsWithCounts[pos + 1] = rightCount
                    }

                    else -> {
                        val sameDirCount = nextPositionsWithCounts.getOrDefault(pos, 0L) + count
                        nextPositionsWithCounts[pos] = sameDirCount
                    }
                }
            }

            nextPositionsWithCounts
        }.values.sum()

    println("Total paths: $totalPaths")
}


main()