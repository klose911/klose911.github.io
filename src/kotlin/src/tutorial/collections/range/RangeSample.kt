package tutorial.collections.range

class Version(val major: Int, val minor: Int): Comparable<Version> {
    override fun compareTo(other: Version): Int {
        if (this.major != other.major) {
            return this.major - other.major
        }
        return this.minor - other.minor
    }
}

fun main() {
    for (i in 1..4)
        print(i)
    println()

    for (i in 4 downTo 1)
        print(i)
    println()

    for (i in 1..8 step 2)
        print(i)

    println()
    for (i in 8 downTo 1 step 2)
        print(i)

    println()
    for (i in 1 until 10)       // i in [1, 10), 10被排除
        print(i)

    println()
    val versionRange = Version(1, 11)..Version(1, 30)
    println(Version(0, 9) in versionRange)
    println(Version(1, 20) in versionRange)

    for (i in 1..9 step 3)
        print(i) // 最后一个元素是 7
    println()

    println((1..10).filter {
        it % 2 == 0
    })

}