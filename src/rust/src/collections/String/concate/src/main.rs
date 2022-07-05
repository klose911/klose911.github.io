#![allow(unused_variables)]
fn main() {
    let s1 = String::from("Hello, ");
    let s2 = String::from("world!");
    let s3 = s1 + &s2; // 注意 s1 被移动了，不能继续使用

    //println!("s1 is {}", s1); s1已经不可用
    println!("s2 is {}", s2);
    println!("s3 is {}", s3);
    
    let s1 = String::from("tic");
    let s2 = String::from("tac");
    let s3 = String::from("toe");

    let s = format!("{}-{}-{}", s1, s2, s3);

    println!("s1 is {}", s1);
    println!("s2 is {}", s2);
    println!("s3 is {}", s3);
    println!("s is {}", s);

}
