package main

import (
	"fmt"
	"math"
)

type Vertex struct {
	X, Y float64
}

// (v *Vertex)作为方法接收者出现在func关键字和方法名中间
func (v *Vertex) Abs() float64 {
	return math.Sqrt(v.X*v.X + v.Y*v.Y)
}

func main() {
	//v是一个结构体Vertex的指针
	v := &Vertex{3, 4}
	//5
	fmt.Println(v.Abs())
}
