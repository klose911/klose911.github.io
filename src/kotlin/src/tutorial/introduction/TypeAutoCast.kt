package tutorial.introduction

fun getStringLength1(obj: Any): Int? {
    if (obj is String) {
        // `obj` 在该条件分支内自动转换成 `String`
        return obj.length
    }

    // 在离开类型检测分支后，`obj` 仍然是 `Any` 类型
    return null
}

fun getStringLength2(obj: Any): Int? {
    if (obj !is String) return null

    // `obj` 在这一分支自动转换为 `String`
    return obj.length
}

fun getStringLength3(obj: Any): Int? {
    // `obj` 在 `&&` 右边自动转换成 `String` 类型
    if (obj is String && obj.length > 0) {
        return obj.length
    }

    return null
}

fun main() {
    fun printLength(obj: Any) {
        println("'$obj' string length is ${getStringLength1(obj) ?: "... err, not a string"} ")
        println("'$obj' string length is ${getStringLength2(obj) ?: "... err, not a string"} ")
        println("'$obj' string length is ${getStringLength3(obj) ?: "... err, not a string"} ")
    }
    printLength("Incomprehensibilities") // 'Incomprehensibilities' string length is 21
    printLength(1000) // '1000' string length is ... err, not a string
    printLength(listOf(Any())) // '[java.lang.Object@2d3fcdbd]' string length is ... err, not a string
    printLength("")
}
