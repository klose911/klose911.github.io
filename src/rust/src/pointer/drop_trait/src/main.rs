struct CustomSmartPointer {
    data: String,
}

impl Drop for CustomSmartPointer {
    fn drop(&mut self) {
        println!("Dropping CustomSmartPointer with data `{}`!", self.data);
    }
}

fn main() {
    let c = CustomSmartPointer { data: String::from("my stuff") };
    // c.drop();
    println!("CustomSmartPointers created.");
    drop(c);
    let d = CustomSmartPointer { data: String::from("other stuff") };
    println!("CustomSmartPointer dropped before the end of main.");
}
