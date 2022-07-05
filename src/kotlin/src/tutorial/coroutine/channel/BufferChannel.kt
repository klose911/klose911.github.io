package tutorial.coroutine.channel

import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

fun main() = runBlocking {
    val channel = Channel<Int>(4) // 启动带缓冲的通道
    val sender = launch { // 启动发送者协程
			  repeat(10) {
			      println("Sending $it") // 在每一个元素发送前打印它们
			      channel.send(it) // 将在缓冲区被占满时挂起
			  }
    }
    // 没有接收到东西……只是等待……
    delay(1000)
    sender.cancel() // 取消发送者协程
}
