package main

import "fmt"

func adder() func(int) int {
	sum := 0
	//这个匿名函数可以对sum变量进行访问和赋值，这个函数就像是被绑定在sum变量上
	return func(x int) int {
		sum += x
		return sum
	}
}

func main() {
	pos, neg := adder(), adder()
	// 0 0
	// 1 -2
	// 3 -6
	// 6 -12
	// 10 -20
	// 15 -30
	// 21 -42
	// 28 -56
	// 36 -72
	// 45 -90
	for i := 0; i < 10; i++ {
		fmt.Println(
			pos(i),
			neg(-2*i),
		)
	}
}
