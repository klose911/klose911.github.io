package tutorial.collections.overview

fun main() {
    val numbersMap = mapOf("key1" to 1, "key2" to 2, "key3" to 3, "key4" to 1)

    println("All keys: ${numbersMap.keys}")
    println("All values: ${numbersMap.values}")
    if ("key2" in numbersMap)
        println("Value by key \"key2\": ${numbersMap["key2"]}")
    if (1 in numbersMap.values)
        println("The value 1 is in the map")
    if (numbersMap.containsValue(1))
        println("The value 1 is in the map")

    val anotherMap = mapOf("key2" to 2, "key1" to 1, "key4" to 1, "key3" to 3)
    println("The maps are equal: ${numbersMap == anotherMap}")

    val mutableNumbersMap = mutableMapOf("one" to 1, "two" to 2)
    mutableNumbersMap.put("three", 3)
    mutableNumbersMap["one"] = 11

    println(mutableNumbersMap)
}