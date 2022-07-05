package main

import (
	"fmt"
	"math"
)

type Vertex struct {
	X, Y float64
}

func (v *Vertex) Scale(f float64) {
	v.X = v.X * f
	v.Y = v.Y * f
}

func (v *Vertex) Abs() float64 {
	return math.Sqrt(v.X*v.X + v.Y*v.Y)
}

func main() {
	v := &Vertex{3, 4}
	//Before scaling: &{X:3 Y:4}, Abs: 5
	fmt.Printf("Before scaling: %+v, Abs: %v\n", v, v.Abs())
	v.Scale(5)
	//After scaling: &{X:15 Y:20}, Abs: 25
	fmt.Printf("After scaling: %+v, Abs: %v\n", v, v.Abs())
}
