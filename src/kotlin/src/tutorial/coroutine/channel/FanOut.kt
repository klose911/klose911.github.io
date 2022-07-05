package tutorial.coroutine.channel

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.channels.ReceiveChannel
import kotlinx.coroutines.channels.produce
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    val producer = produceNumbers1()
    repeat (5) {
        launchProcessor(it, producer)
    }
    delay(950)
    producer.cancel() // 取消协程生产者从而将它们全部杀死
}

fun CoroutineScope.produceNumbers1() = produce {
    var x = 1 // start from 1
    while (true) {
        send(x++) // 产生下一个数字
        delay(100) // 等待 0.1 秒
    }
}

fun CoroutineScope.launchProcessor(id: Int, channel: ReceiveChannel<Int>) = launch {
    for (msg in channel) {
        println("Processor #$id received $msg")
    }
}
