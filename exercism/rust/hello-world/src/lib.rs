pub fn hello(nameOpt: Option<&str>) -> String {
    let name = match nameOpt {
        Some(s) => s,
        None => "World",
    };

    "Hello, ".to_string() + name + "!"
}
