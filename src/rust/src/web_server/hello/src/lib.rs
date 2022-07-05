use std::thread;
use std::sync::mpsc;
use std::sync::Arc;
use std::sync::Mutex;

enum Message {
    NewJob(Job),
    Terminate,
}

pub struct ThreadPool {
    workers: Vec<Worker>,
    sender: mpsc::Sender<Message>,
}

type Job = Box<dyn FnOnce() + Send + 'static>;

impl ThreadPool {
    /// 创建线程池。
    ///
    /// 线程池中线程的数量。
    ///
    /// # Panics
    ///
    /// `new` 函数在 size 为 0 时会 panic。
    pub fn new(size: usize) -> ThreadPool {
	assert!(size > 0);
	
        let (sender, receiver) = mpsc::channel();
        let receiver = Arc::new(Mutex::new(receiver));

        let mut workers = Vec::with_capacity(size);
	
	
        for id in 0..size {
            workers.push(Worker::new(id, Arc::clone(&receiver)));
        }
	
	ThreadPool {
	    workers,
	    sender
	}
    }

    pub fn execute<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static
    {
        let job = Box::new(f);

	self.sender.send(Message::NewJob(job)).unwrap();
    }
}


impl Drop for ThreadPool {
    fn drop(&mut self) {
	println!("Sending terminate message to all workers.");

	for _ in &mut self.workers {
	    self.sender.send(Message::Terminate).unwrap();
	}

	println!("Shutting down all workers.");

        for worker in &mut self.workers {
            println!("Shutting down worker {}", worker.id);

	    if let Some(thread) = worker.thread.take() {
                thread.join().unwrap();
            }
        }
    }
}

struct Worker {
    id: usize,
    thread: Option<thread::JoinHandle<()>>,
}

impl Worker { 
    fn new(id: usize, receiver: Arc<Mutex<mpsc::Receiver<Message>>>) -> Worker {

	// let thread = thread::spawn(move || {
        //     while let Ok(job) = receiver.lock().unwrap().recv() {
        //         println!("Worker {} got a job; executing.", id);

	// while block always hold lock, lock will not be released before job has finished 
        //         job();
        //     }
        // });
	
	let thread = thread::spawn(move || {
            loop {
                // let job = receiver.lock().unwrap().recv().unwrap();

                // println!("Worker {} got a job; executing.", id);

		// // lock is already released 
                // job();
		let message = receiver.lock().unwrap().recv().unwrap();

		match message {
		    Message::NewJob(job) => {
			println!("Worker {} got a job; executing.", id);

			job();
		    },
		    Message::Terminate => {
			println!("Worker {} was told to terminate.", id);

			break;
		    },
		}
            }
        });

        Worker {
            id,
            thread: Some(thread) 
        }
    }
}
