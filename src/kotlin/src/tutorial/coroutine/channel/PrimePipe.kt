package tutorial.coroutine.channel

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.cancelChildren
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.channels.produce
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    var cur = numbersFrom(2)
    repeat(10) {
        val prime = cur.receive()
        println(prime)
        cur = filter(cur, prime)
    }

    coroutineContext.cancelChildren() // 取消所有的子协程来让主协程结束
}

fun CoroutineScope.numbersFrom(start: Int) = produce {
    var x = start
    while (true)
        send(x++) // 从 start 开始过滤整数流
}

fun CoroutineScope.filter(numbers: ReceiveChannel<Int>, prime: Int) = produce {
    for (x in numbers)
        if (x % prime != 0)
            send(x)
} 
