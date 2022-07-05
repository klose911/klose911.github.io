package tutorial.collections.overview

fun main() {
    val numbers = mutableListOf("one", "two", "three", "four")
    numbers.add("five")   // 这是可以的
//    numbers = mutableListOf("six", "seven")      // 编译错误
}