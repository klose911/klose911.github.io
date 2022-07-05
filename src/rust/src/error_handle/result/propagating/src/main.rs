#![allow(unused_variables)]
use std::error::Error;
use std::fs::File;

fn main() -> Result<(), Box<dyn Error>> {
    use std::io;
    use std::io::Read;
    
    fn read_username_from_file() -> Result<String, io::Error> {
	let f = File::open("hello.txt");

	let mut f = match f {
            Ok(file) => file,
            Err(e) => return Err(e),
	};

	let mut s = String::new();

	match f.read_to_string(&mut s) {
            Ok(_) => Ok(s),
            Err(e) => Err(e),
	}
    }

    fn read_username_from_file_with_question() -> Result<String, io::Error> {
	let mut f = File::open("hello.txt")?;
	let mut s = String::new();
	f.read_to_string(&mut s)?;
	Ok(s)
    }

    fn read_username_from_file_flow_call() -> Result<String, io::Error> {
	let mut s = String::new();

	File::open("hello.txt")?.read_to_string(&mut s)?;

	Ok(s)
    }

    use std::fs;
    fn read_username_from_file_with_std_lib() -> Result<String, io::Error> {
	fs::read_to_string("hello.txt")
    }

    
    let f = File::open("hello.txt")?;

    Ok(())
}

// fn main() {
//     let f = File::open("hello.txt")?;

//}
