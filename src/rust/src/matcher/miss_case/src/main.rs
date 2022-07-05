fn main() {    
    fn plus_one(x: Option<i32>) -> Option<i32> {
	match x {
            Some(i) => Some(i + 1),
	}
    }
}
