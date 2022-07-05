package tutorial.collections.constructor

fun main() {
    val sourceList1 = mutableListOf(1, 2, 3)
    val copyList1 = sourceList1.toMutableList()
    val readOnlyCopyList1 = sourceList1.toList()
    sourceList1.add(4)
    println("Copy size: ${copyList1.size}")

    //readOnlyCopyList.add(4)             // 编译异常
    println("Read-only copy size: ${readOnlyCopyList1.size}")

    val sourceList2 = mutableListOf(1, 2, 3)
    val copySet2 = sourceList2.toMutableSet()
    copySet2.add(3)
    copySet2.add(4)
    println(copySet2)

    val sourceList3 = mutableListOf(1, 2, 3)
    val referenceList3 = sourceList3
    referenceList3.add(4)
    println("Source size: ${sourceList3.size}")

    val sourceList4 = mutableListOf(1, 2, 3)
    val referenceList4: List<Int> = sourceList4
    //referenceList4.add(4)            // 编译错误
    sourceList4.add(4)
    println(referenceList4) // 显示 sourceList 当前状态
}