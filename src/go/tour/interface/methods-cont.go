package main

import (
	"fmt"
	"math"
)

//定义新的类型MyFloat
type MyFloat float64

//Abs方法作用于MyFloat类型上
func (f MyFloat) Abs() float64 {
	if f < 0 {
		return float64(-f)
	}
	return float64(f)
}

func main() {
	//-1.4142135623730951
	f := MyFloat(-math.Sqrt2)
	//1.4142135623730951
	fmt.Println(f.Abs())
}
