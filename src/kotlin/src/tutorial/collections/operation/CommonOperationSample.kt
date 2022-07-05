package tutorial.collections.operation

fun main() {
    val numbers = listOf("one", "two", "three", "four")
    numbers.filter { it.length > 3 }  // `numbers` 没有任何改变，结果丢失
    println("numbers are still $numbers")
    val longerThan3 = numbers.filter { it.length > 3 } // 结果存储在 `longerThan3` 中
    println("numbers longer than 3 chars are $longerThan3")


    val filterResults = mutableListOf<String>()  // 目标对象
    numbers.filterTo(filterResults) { it.length > 3 }
    numbers.filterIndexedTo(filterResults) { index, _ -> index == 0 }
    println(filterResults) // 包含两个操作的结果

    // 将数字直接过滤到新的哈希集中，
    // 从而消除结果中的重复项
    val result = numbers.mapTo(HashSet()) { it.length }
    println("distinct item lengths are $result")
}