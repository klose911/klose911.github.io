package main

import (
	"fmt"
	"math"
)

func compute(fn func(float64, float64) float64) float64 {
	return fn(3, 4)
}

func main() {
	//声明一个函数
	hypot := func(x, y float64) float64 {
		return math.Sqrt(x*x + y*y)
	}
	//13 调用hypot函数：sqrt(5 * 5 + 12 * 12)
	fmt.Println(hypot(5, 12))
	//5 把hypot函数作为参数传递给compute: sqrt(3*3 + 4 * 4)
	fmt.Println(compute(hypot))
	//81 把Math.Pow作为参数传递给compute: pow(3, 4)
	fmt.Println(compute(math.Pow))
}
