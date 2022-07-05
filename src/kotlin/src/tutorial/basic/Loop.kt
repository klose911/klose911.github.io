package tutorial.basic

fun main() {
    for (i in 1..3) {
        println(i)
    }
    for (i in 6 downTo 0 step 2) {
        println(i)
    }

    val array = arrayOf("a", "b", "c")
    for (i in array.indices) {
        println(array[i])
    }

    for ((index, value) in array.withIndex()) {
        println("the element at $index is $value")
    }
}