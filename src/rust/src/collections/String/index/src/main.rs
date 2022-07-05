fn main() {
    //let s1 = String::from("hello");
    // not support index
    //let h = s1[0]; 

    let len = String::from("Hola").len();
    println!("len is {}", len); // 4  
    let len = String::from("Здравствуйте").len();
    println!("len is {}", len); // 24

    let hello = "Здравствуйте";
    let s = &hello[0..4];
    println!("slice is {}", s); // "Зд"

    let s = &hello[0..1];
    println!("slice is {}", s); // panic
}
