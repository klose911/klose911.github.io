package tutorial.coroutine.select

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.SendChannel
import kotlinx.coroutines.channels.consumeEach
import kotlinx.coroutines.channels.produce
import kotlinx.coroutines.selects.select

suspend fun CoroutineScope.produceNumbers(side: SendChannel<Int>) = produce<Int> {
    for (num in 1..10) {
        // 生产从 1 到 10 的 10 个数值
        delay(100) // 延迟 100 毫秒
        select<Unit> {
            onSend(num) {} // 发送到主通道
            side.onSend(num) {} // 或者发送到 side 通道
        }
    }
}


fun main() = runBlocking {
    val side = Channel<Int>() // 分配 side 通道
    launch {
        // 对于 side 通道来说，这是一个很快的消费者
        side.consumeEach {
            println("Side channel has $it")
        }
    }

    produceNumbers(side).consumeEach {
        println("Consuming $it")
        delay(250) // 不要着急，让我们正确消化消耗被发送来的数字
    }

    println("Done consuming")
    coroutineContext.cancelChildren()
} 
