package main

import "fmt"

// 1
// 2
// 4
// 8
// 16
// 32
// 64
// 128
// 256
// 512
func main() {
	pow := make([]int, 10)
	//如果只需要索引值，去掉 “ , value ” 的部分
	for i := range pow {
		pow[i] = 1 << uint(i)
	}
	//通过赋值给 _ 来忽略序号和值
	for _, value := range pow {
		fmt.Printf("%d\n", value)
	}
}
