package tutorial.coroutine.channel

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.cancelChildren
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.channels.produce
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    val numbers = produceNumbers() // 从 1 开始生成整数
    val squares = square(numbers) // 整数求平方
    repeat(5) {
        println(squares.receive()) // 输出前五个
    }
    println("Done!") // 至此已完成
    coroutineContext.cancelChildren() // 取消子协程
}

fun CoroutineScope.produceNumbers() = produce {
    var x = 1
    while (true)
        send(x++) // 从 1 开始的无限的整数流
}

fun CoroutineScope.square(numbers: ReceiveChannel<Int>): ReceiveChannel<Int> = produce {
    for (x in numbers)
        send(x * x)
}
