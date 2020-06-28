use rustyline::{error::ReadlineError, Editor};

#[no_mangle]
extern "C" fn assert(cond: bool) {
    assert!(cond);
}

#[no_mangle]
extern "C" fn trace(s: *const u8) {
    let s = unsafe { std::ffi::CStr::from_ptr(s as *const std::os::raw::c_char) };
    eprintln!("TRACE: {}", s.to_string_lossy());
}

#[no_mangle]
extern "C" fn trace_i32(s: *const u8, value: i32) {
    let s = unsafe { std::ffi::CStr::from_ptr(s as *const std::os::raw::c_char) };
    eprintln!("TRACE: {} (value={})", s.to_string_lossy(), value);
}

#[link(name = "calc", kind = "static")]
extern "C" {
    fn calc_str(text: *const u8, text_len: usize) -> i32;
}

fn main() {
    let mut rl: Editor<()> = Editor::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.to_string());

                let result = unsafe { calc_str(line.as_ptr(), line.len()) };
                println!("{}", result);
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }
}
