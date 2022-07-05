package main

import (
	"fmt"
	"math"
)

func pow(x, n, lim float64) float64 {
	if v := math.Pow(x, n); v < lim {
		return v
	} else {
		fmt.Printf("%g >= %g\n", v, lim)
	}
	// 这里开始就不能使用 v 了
	return lim
}

//两个 pow 调用都在 main 调用 fmt.Println 前执行完毕了
func main() {
	//27 >= 20
	//9 20
	fmt.Println(
		pow(3, 2, 10),
		pow(3, 3, 20),
	)
}
