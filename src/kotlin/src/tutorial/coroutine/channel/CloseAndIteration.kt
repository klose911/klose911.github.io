package tutorial.coroutine.channel

import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    val channel = Channel<Int>()
    launch {
        for (x in 1..5)
            channel.send(x * x)
        channel.close() // 我们结束发送
    }
    // 这里我们使用 `for` 循环来打印所有被接收到的元素（直到通道被关闭）
    for (y in channel)
        println(y)
    println("Done!")
} 
