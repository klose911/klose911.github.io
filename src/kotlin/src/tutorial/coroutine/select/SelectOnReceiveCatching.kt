package tutorial.coroutine.select

import kotlinx.coroutines.cancelChildren
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.channels.produce
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.selects.select

suspend fun selectAorB(a: ReceiveChannel<String>, b: ReceiveChannel<String>): String =
    select {
        a.onReceiveCatching { it ->
            val value = it.getOrNull()
            if (value != null) {
                "a -> '$value'"
            } else {
                "Channel 'a' is closed"
            }
        }
        b.onReceiveCatching { it ->
            val value = it.getOrNull()
            if (value != null) {
                "b -> '$value'"
            } else {
                "Channel 'b' is closed"
            }
        }
    }

fun main() = runBlocking {
    val a = produce {
        repeat(4) {
            send("Hello $it")
        }
    }
    val b = produce {
        repeat(4) {
            send("World $it")
        }
    }
    repeat(8) { // 打印最早的八个结果
        println(selectAorB(a, b))
    }
    coroutineContext.cancelChildren()
} 
