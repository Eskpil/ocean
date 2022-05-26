use std::process::Command;

pub fn run_cmd_echoed(input: String) {
    let parts = input.split(" ");
    let mut args = Vec::<String>::new();

    for part in parts {
        args.push(part.to_string());
    }    

    println!("[Info]: Running command: \"{}\"", input);

    let cmd = Command::new(args[0].clone())
        .args({ 
            let mut args = args.clone(); 
            args.remove(0);
            args
        })
        .output()
        .expect(format!("Failed to execute process: {}", args[0].clone()).as_str());
}
