use std::io::Write;

pub fn print_indent(indent: usize, input: String) {
    let repeated = std::iter::repeat(" ").take(indent * 2).collect::<String>();
    let message = format!("{}{}", repeated, input);

    writeln!(std::io::stdout(), "{message}").unwrap();
}
