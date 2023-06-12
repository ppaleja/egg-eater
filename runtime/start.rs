use std::env;
use std::arch::asm;
#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input : i64, memory : *mut i64) -> i64;
}

#[no_mangle]
#[export_name = "\x01snek_error"]
pub fn snek_error(errcode : i64) {
    eprintln!("An error occurred: {}", match errcode {
        7 => "invalid argument!",
        8 => "index out of bounds!",
        9 => "null pointer exception!",
        _ => "unknown errcode",
    });
    // TODO:
    // 7 means invalid operands
    // 8 means index out of bounds.
    // 9 means null pointer exception
    std::process::exit(1);
}

// let's change snek_str to print ... when it visits a cyclic value
fn snek_str(val : i64, seen : &mut Vec<i64>) -> String {
    if val == 7 { "true".to_string() }
    else if val == 3 { "false".to_string() }
    else if val % 2 == 0 { format!("{}", val >> 1) }
    else if val == 1 { "nil".to_string() }
    else if val & 1 == 1 {
        if seen.contains(&val)  { return "(tuple <cyclic>)".to_string() }
        seen.push(val);
        let addr = (val - 1) as *const i64;
        
        // Note: pair will NOT print correctly anymore
        let size: i64 = unsafe { *addr } / 2;
        let mut result = "(tuple".to_string();
        for i in 1..(size + 1) {
            let el = unsafe { *addr.offset(i as isize) };
            result += format!(" {}", snek_str(el, seen)).as_str();
        }
        seen.pop();
        result + ")"
    }
    else {
        format!("Unknown value: {}", val)
    }
}

// let's change snek_str to print ... when it visits a cyclic value
#[no_mangle]
#[export_name = "\x01snek_eq"]
pub extern "C" fn snek_eq(val1: i64, val2: i64) -> i64 {
    snek_eq_mut(val1, val2, &mut Vec::new())
}


fn snek_eq_mut(val1 : i64, val2 : i64, seen : &mut Vec<(i64, i64)>) -> i64 {

    if (val1 & 7) != (val2 & 7) {
        return 3; // Different types
    }

    // If not tuples, then we don't have to do anything special
    if (val1 & 1 != 1) || (val2 & 1 != 1) {
        return if val1 == val2 {7} else {3};
    }

    // If nil, then we don't have to do anything special
    if (val1 == 1) || (val2 == 1) {
        return if val1 == val2 {7} else {3};
    }

    // Now we know that they are:
    //   - Same Types
    //   - Tuples
    //   - Non-Nil

    if seen.contains(&(val1, val2))  { return 7 }
    seen.push((val1, val2));
    let addr1 = (val1 - 1) as *const i64;
    let addr2= (val2 - 1) as *const i64;
    
    // Note: pair will NOT print correctly anymore
    let size1: i64 = unsafe { *addr1 } / 2;
    let size2: i64 = unsafe { *addr2 } / 2;

    if size1 != size2 {
        return 3;
    }

    for i in 1..(size1 + 1) {
        let el1 = unsafe { *addr1.offset(i as isize) };
        let el2 = unsafe { *addr2.offset(i as isize) };
        if snek_eq_mut(el1, el2, seen) == 3 {
            return 3;
        }
    }
    seen.pop();
    return 7;
    
}


#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val : i64) -> i64 {
    let mut seen = Vec::<i64>::new();
    println!("{}", snek_str(val, &mut seen));
    return val;
}
fn parse_arg(v : &Vec<String>) -> i64 {
    if v.len() < 2 { return 1 }
    let s = &v[1];
    if s == "true" { 7 }
    else if s == "false" { 3 }
    else { s.parse::<i64>().unwrap() << 1 }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = parse_arg(&args);

    let mut memory = Vec::<i64>::with_capacity(1000000);
    let buffer :*mut i64 = memory.as_mut_ptr();
    
    let i : i64 = unsafe { our_code_starts_here(input, buffer) };
    snek_print(i);
}


