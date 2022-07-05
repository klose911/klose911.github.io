// version 1 
// pub trait Summary {
//     fn summarize(&self) -> String;
// }

// version 2 
// pub trait Summary {
//     fn summarize(&self) -> String {
//         String::from("(Read more...)")
//     }
// }

// final version 
pub trait Summary {
    fn summarize_author(&self) -> String;

    fn summarize(&self) -> String {
        format!("(Read more from {}...)", self.summarize_author())
    }
} 
pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

// version 1 
// impl Summary for NewsArticle {
//     fn summarize(&self) -> String {
// 	format!("{}, by {} ({})", self.headline, self.author, self.location)
//     }
// }

// version 2 
// impl Summary for NewsArticle {
// }

impl Summary for NewsArticle {
    fn summarize_author(&self) -> String {
	format!("@{}", self.author)
    }
}

pub struct Tweet {
    pub username: String,
    pub content: String,
    pub reply: bool,
    pub retweet: bool,
}

// version 1 & 2 
// impl Summary for Tweet {
//     fn summarize(&self) -> String {
//         format!("{}: {}", self.username, self.content)
//     }
// }

impl Summary for Tweet {
    fn summarize_author(&self) -> String {
        format!("@{}", self.username)
    }
}

pub fn notify(item: impl Summary) {
    println!("Breaking news! {}", item.summarize());
}

fn returns_summarizable() -> impl Summary {
    Tweet {
        username: String::from("horse_ebooks"),
        content: String::from("of course, as you probably already know, people"),
        reply: false,
        retweet: false,
    }
}

// could return only one exactly type 
// fn returns_summarizable(switch: bool) -> impl Summary {
//     if switch {
//         NewsArticle {
//             headline: String::from("Penguins win the Stanley Cup Championship!"),
//             location: String::from("Pittsburgh, PA, USA"),
//             author: String::from("Iceburgh"),
//             content: String::from("The Pittsburgh Penguins once again are the best
//             hockey team in the NHL."),
//         }
//     } else {
//         Tweet {
//             username: String::from("horse_ebooks"),
//             content: String::from("of course, as you probably already know, people"),
//             reply: false,
//             retweet: false,
//         }
//     }
// }

fn main() {
    let tweet = Tweet {
	username: String::from("horse_ebooks"),
	content: String::from("of course, as you probably already know, people"),
	reply: false,
	retweet: false,
    };

    println!("1 new tweet: {}", tweet.summarize());
    notify(tweet);
    
    let article = NewsArticle {
	headline: String::from("Penguins win the Stanley Cup Championship!"),
	location: String::from("Pittsburgh, PA, USA"),
	author: String::from("Iceburgh"),
	content: String::from("The Pittsburgh Penguins once again are the best
    hockey team in the NHL."),
    };

    println!("New article available! {}", article.summarize());
    notify(article); 
}
